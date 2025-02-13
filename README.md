# fhir.package

Utility functions to work with FHIR packages.
Create cache of packages in .fhir-packages directory.


```clj

(def r4-pkg (fhir.package/pkg-info "hl7.fhir.r4.core@4.0.1"))

{:name "hl7.fhir.r4.core",
 :version "4.0.1",
 :dist {:tarball string?}})

(fhir.package/reduce-package r4-pkg
 (fn [acc file-name read-fn]
    (if (str/ends-with? file-name ".json")
      (let [res (read-fn true)]
        (if (and (:resourceType res) (:url res))
          (assoc-in acc [(:resourceType res) (:url res) (:version res)] res)
          acc))
      acc)))

```


