(ns fhir.package-test
  (:require [fhir.package :as subj]
            [matcho.core :as matcho]
            [clojure.string :as str]
            [clojure.test :as t]))

(t/deftest test-fhir-packages

  (matcho/match
      (subj/pkg-info "hl7.fhir.r4.core")
    {:name "hl7.fhir.r4.core",
     :versions
     {:4.0.1
      {:name "hl7.fhir.r4.core"
       :version "4.0.1"}}})

  (t/is (= (subj/get-tarball-url (subj/pkg-info "hl7.fhir.r4.core"))
           "https://packages.simplifier.net/hl7.fhir.r4.core/4.0.1"))


  (def pkg-info (subj/pkg-info "hl7.fhir.r4.core@4.0.1"))
  (matcho/match
      pkg-info
    {:name "hl7.fhir.r4.core",
     :version "4.0.1",
     :dist {:tarball string?}})

  (t/is (= (subj/get-tarball-url pkg-info)
           "https://packages.simplifier.net/hl7.fhir.r4.core/4.0.1"))

  (t/is (thrown?  Exception (subj/pkg-info "hl7.fhir.r4.unexisting")))

  (def bulp-pkg (subj/pkg-info "ihe.iti.balp"))

  (subj/load-tarball bulp-pkg)

  (matcho/match
      (->> (subj/reduce-package bulp-pkg (fn [acc file-name _read-fn] (conj acc file-name)) [])
           sort
           (take 10))
    [".index.db"
     ".index.json"
     "CapabilityStatement-IHE.BALP.ATNA.AuditRecordRepository.json"
     "CapabilityStatement-IHE.BALP.AuditConsumer.json"
     "CapabilityStatement-IHE.BALP.AuditCreator.json"
     "CodeSystem-AuthZsubType.json"
     "CodeSystem-BasicAuditEntityType.json"
     "CodeSystem-OtherIdentifierTypes.json"
     "CodeSystem-UserAgentTypes.json"
     "ImplementationGuide-ihe.iti.balp.json"])


  (def r4-pkg (subj/pkg-info "hl7.fhir.r4.core@4.0.1"))

  (matcho/match
      (->> (subj/reduce-package r4-pkg (fn [acc file-name _read-fn] (conj acc file-name)) [])
           sort
           (take 10))
    [".index.json"
     "CapabilityStatement-base.json"
     "CapabilityStatement-base2.json"
     "CapabilityStatement-example.json"
     "CapabilityStatement-knowledge-repository.json"
     "CapabilityStatement-measure-processor.json"
     "CapabilityStatement-messagedefinition.json"
     "CapabilityStatement-phr.json"
     "CapabilityStatement-terminology-server.json"
     "CodeSystem-FHIR-version.json"])

  (matcho/match
      (subj/package-json bulp-pkg)
    {:url "https://profiles.ihe.net/ITI/BALP/1.1.3",
     :version "1.1.3",
     :dependencies
     {:hl7.fhir.r4.core "4.0.1",
      :hl7.terminology.r4 "5.3.0",
      :hl7.fhir.uv.extensions.r4 "1.0.0"}})

  (subj/index-json bulp-pkg)

  (def r4-idx
    (subj/reduce-package r4-pkg (fn [acc file-name read-fn]
                                  (if (str/ends-with? file-name ".json")
                                    (let [res (read-fn true)]
                                      (if (and (:resourceType res) (:url res))
                                        (assoc-in acc [(:resourceType res) (:url res) (:version res)] res)
                                        acc))
                                    acc))))

  (keys (get bulp-idx "ValueSet"))

  )
