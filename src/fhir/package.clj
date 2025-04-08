(ns fhir.package
  (:require
   [cheshire.core]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.test :as t])
  (:import
   [java.net URL]
   [java.io InputStream]
   [org.apache.commons.compress.archivers.tar TarArchiveEntry TarArchiveInputStream]
   [org.apache.commons.compress.compressors.gzip GzipCompressorInputStream]))

(set! *warn-on-reflection* true)

(def SIMPLIFIER_REPO "https://packages.simplifier.net")
(def HL7_REPO "https://packages2.fhir.org/packages")
(def GET_IG_REPO "https://get-ig.org")

(defn get-registry [] GET_IG_REPO)

(defn get-tarball-url [pkg-info]
  (or (get-in pkg-info [:dist :tarball])
      (when-let [v (get-in pkg-info [:dist-tags :latest])]
        (get-in pkg-info [:versions (keyword v) :dist :tarball]))))

(defn get-cache-file-name [pkg-info]
  (let [nm (:name pkg-info)
        v (or (:version pkg-info) (get-in pkg-info [:dist-tags :latest]))]
    (str nm "@" v ".tar.gz")))

(defn load-tarball [pkg-info]
  (let [project-dir (System/getProperty "user.dir")
        cache-file-name (get-cache-file-name pkg-info)
        file-path (str project-dir "/.fhir-packages/" cache-file-name)
        file (io/file file-path)
        ^URL url (URL. (get-tarball-url pkg-info))]
    (io/make-parents file-path)
    ;; cache file
    (when-not (.exists file)
      (with-open [^InputStream in (.openStream url)
                  out (io/output-stream file-path)]
        (io/copy in out)))
    file-path))

(defn reduce-tar [pkg-info cb & [acc]]
  (let [cache-file (load-tarball pkg-info)]
    (with-open [^InputStream input-stream (io/input-stream cache-file)
                ^GzipCompressorInputStream gzip-compressor-input-stream (GzipCompressorInputStream. input-stream)
                ^TarArchiveInputStream tar-archive-input-stream (TarArchiveInputStream. gzip-compressor-input-stream)]
      (loop [acc (or acc {})]
        (if-let [^TarArchiveEntry entry (.getNextTarEntry tar-archive-input-stream)]
          (let [^String nm (str/replace (.getName entry) #"package/" "")
                read-fn (fn [& [json]]
                          (let [content (byte-array (.getSize entry))]
                            (.read tar-archive-input-stream content)
                            (if json
                              (cheshire.core/parse-string (String. content) keyword)
                              (String. content))))]
            (recur (cb acc nm read-fn)))
          acc)))))

(defn try-resolve-tag [package-info version-or-tag]
  (get-in package-info [:dist-tags (keyword version-or-tag)]))

#_(t/with-test
  (defn extract-package-version-manifest [manifest version-str]
    )
  (let [tpkg {:dist-tags {:latest "8.0.0-ballot"},
              :versions {:0.0.0 {:version "0.0.0"},
                         :1.0.0 {:version "1.0.0"},
                         :1.0.1 {:version "1.0.1"},
                         :1.1.0 {:version "1.1.0"},
                         :2.0.0 {:version "2.0.0"},
                         :2.1.0 {:version "2.1.0"},
                         :3.0.0 {:version "3.0.0"},
                         :3.0.1 {:version "3.0.1"},
                         :3.1.0 {:version "3.1.0"},
                         :3.1.1 {:version "3.1.1"}
                         :3.2.0 {:version "3.2.0"},
                         :4.0.0 {:version "4.0.0"},
                         :4.1.0 {:version "4.1.0"},
                         :5.0.0 {:version "5.0.0"},
                         :5.0.1 {:version "5.0.1"},
                         :6.0.0 {:version "6.0.0"},
                         :6.0.0-ballot {:version "6.0.0-ballot"},
                         :6.1.0 {:version "6.1.0"},
                         :6.1.0-snapshot1 {:version "6.1.0-snapshot1"},
                         :7.0.0 {:version "7.0.0"},
                         :7.0.0-ballot {:version "7.0.0-ballot"},
                         :8.0.0-ballot {:version "8.0.0-ballot"}}
              :version "8.0.0-ballot"}]
    (t/is (= ;; use dist-tags
           {:version "8.0.0-ballot"}
           (extract-package-version-manifest tpkg "latest")
           ))

    (t/is (= ;; because no release
           {:version "8.0.0-ballot"}
           (extract-package-version-manifest tpkg "8.0.x")))

    (t/is (= ;; because it's the last release
           {:version "7.0.0"}
           (extract-package-version-manifest tpkg "7.0.x")))

    (t/is (= {:version "3.0.1"}
             (extract-package-version-manifest tpkg "3.0.x")))

    (t/is (= {:version "3.2.0"}
             (extract-package-version-manifest tpkg "3.x")))

    (t/is (= {:version "8.0.0-ballot"}
             (extract-package-version-manifest tpkg "x")))

    )
  )

(defn pkg-info
  "get package information pkg could be just name a.b.c or versioned name a.b.c@1.0.0"
  [pkg & [registry]]
  (let [[pkg version-or-tag] (str/split pkg #"@" 2)
        version-or-tag (or version-or-tag "latest")
        package-info (-> (slurp (-> (str (or registry (get-registry)) "/" pkg)
                                    (URL.)
                                    (.openConnection)
                                    (doto (.setRequestProperty "Accept" "application/json"))
                                    (.getInputStream)))
                         (cheshire.core/parse-string keyword))
        version (or (try-resolve-tag package-info version-or-tag)
                    version-or-tag)]

    (when (nil? version)
      (throw (ex-info (format "Can't find specified version %s for package %s"
                              version-or-tag
                              pkg) {})))

    (get-in package-info [:versions (keyword version)])))

(defn reduce-package
  "(fn on-file [acc file-name read-resource-fn])"
  [pkg-info on-file & [acc]]
  (reduce-tar pkg-info (fn [acc nm read-resource] (on-file acc nm read-resource)) acc))

(defn package-json [pkg-info]
  (reduce-package
   pkg-info
   (fn [acc file-name read-fn]
     (if (= file-name "package.json")
       (read-fn true)
       acc))))

(defn index-json [pkg-info]
  (reduce-package
   pkg-info
   (fn [acc file-name read-fn]
     (if (= file-name ".index.json")
       (read-fn true)
       acc))))
