(ns jtk-dvlp.lein-tpl-helpers.core
  (:require
   [clojure.set :refer [difference]]
   [clojure.string :as str]
   [clojure.java.io :as io]

   [leiningen.core.main :refer [abort warn info debug]]
   [leiningen.new.templates :as lein-tpl]

   [stencil.core :as stencil]
   [stencil.parser :as stencil-parser]

   ,,,))


(def ^:dynamic *template-dir* nil)
(def ^:dynamic *project-dir* nil)

(defn- relativize
  [dir file]
  (let [dirpath
        (.getAbsolutePath (io/file dir))

        filepath
        (.getAbsolutePath (io/file file))]

    (cond
      (not (str/starts-with? filepath dirpath))
      (->> (format "File \"%s\" is not located in dir \"%s\"" filepath dirpath)
           (IllegalArgumentException.)
           (throw))

      (= dirpath filepath)
      (io/file ".")

      :else
      (-> filepath
          (subs (inc (count dirpath)))
          (io/file)))))

(defn- template-relativize
  [file]
  (-> *template-dir*
      (.getPath)
      (io/resource)
      (relativize file)))

(defn- template-files
  [path]
  (-> (io/file *template-dir* path)
      (.getPath)
      (io/resource)
      (io/file)
      (file-seq)))

(defn- matches-file-ext?
  [ext file]
  (str/ends-with? (.getName file) ext))

(def ^:private clojure-file?
  (partial matches-file-ext? ".clj"))

(def ^:private clojurescript-file?
  (partial matches-file-ext? ".cljs"))

(def ^:private clojure-common-file?
  (partial matches-file-ext? ".cljc"))

(def ^:private edn-file?
  (partial matches-file-ext? ".edn"))

(def ^:private edn-like-file?
  (some-fn edn-file? clojure-file? clojurescript-file? clojure-common-file?))

(defn- render
  [template-src data-map]
  (let [parser-options
        (cond-> stencil-parser/parser-defaults
          (edn-like-file? template-src)
          (assoc :tag-open "<%", :tag-close "%>"))]

    (-> (io/file *template-dir* template-src)
        (.getPath)
        (io/resource)
        (lein-tpl/slurp-resource)
        (stencil-parser/parse parser-options)
        (stencil/render data-map))))

(defn- generate-file!
  [raw-file data]
  (let [rendered-filepath
        (-> raw-file (.getPath) (lein-tpl/render-text data) (io/file))

        absolute-filepath
        (io/file *project-dir* rendered-filepath)]

    (debug (format "Generating file \"%s\"" rendered-filepath))
    (.mkdirs (.getParentFile absolute-filepath))

    (-> raw-file
        (render data)
        (io/copy absolute-filepath))))

(defn- generate-dir!
  [dir data]
  (let [dir (-> dir (.getPath) (lein-tpl/render-text data))]
    (debug (format "Generating dir \"%s\"" dir))
    (.mkdirs (io/file *project-dir* dir))))

(defn ->dir
  [path excludes data]
  (doseq [template-file (template-files path)
          :let [relative-file (template-relativize template-file)]]
    (cond
      ;; TODO: Verzechnisse mit deren Dateien unterstützen
      (excludes (.getPath relative-file))
      (debug "Skip file" (.getPath relative-file))

      (.isFile template-file)
      (generate-file! relative-file data)

      (.isDirectory template-file)
      (generate-dir! relative-file data))))

;; (defn ->files
;;   [data & paths]
;;   (doseq [path paths]
;;     (generate-file!
;;      (io/file *template-dir* path)
;;      (io/file *project-dir* path)
;;      data)))

(defn excludes
  [opts-files opts]
  (let [all-files
        (->> opts-files
             (vals)
             (apply concat)
             (into #{}))

        files-to-include
        (->> opts
             (select-keys opts-files)
             (vals)
             (apply concat)
             (into #{}))]

    (difference all-files files-to-include)))

(defn- stream-to-fn
  [fun stream]
  (->> stream
       (java.io.InputStreamReader.)
       (java.io.BufferedReader.)
       (line-seq)
       (run! fun)))

(defn- exec!
  [& command]
  (let [runtime
        (Runtime/getRuntime)

        process
        (.exec runtime (into-array command) nil *project-dir*)

        std-out
        (.getInputStream process)

        std-err
        (.getErrorStream process)]

    (future (stream-to-fn (partial info "..") std-out))
    (future (stream-to-fn (partial warn "..") std-err))

    (.waitFor process)))

(defn upgrade-dependencies!
  []
  (info "Upgrading template dependencies and running tests.")
  (when-not (= 0 (exec! "lein" "ancient" "upgrade" ":check-clojure"))
    (abort "Upgrading dependencies failed")))

(defn project-data
  [name opts]
  (let [customer
        (lein-tpl/group-name name)

        project
        (lein-tpl/project-name name)

        options
        (reduce
         (fn [m option]
           (assoc m option true))
         {} opts)]

    (merge
     {:product-name name
      :product-title (str (when customer (str customer "-")) project)
      :product-ns (lein-tpl/sanitize-ns name)
      :product-path (lein-tpl/name-to-path name)

      :customer (or customer "TODO")
      :customer-path (lein-tpl/name-to-path (or customer "TODO"))

      :project project
      :project-path (lein-tpl/name-to-path project)}

     options)))

(defn project-dir
  [{:keys [product-title] :as _data-map}]
  (or lein-tpl/*dir*
      (-> (System/getProperty "leiningen.original.pwd")
          (io/file product-title)
          (.getPath))))

(defn generate!
  [feature-files name features]
  (let [template-data (project-data name features)]
    (binding [*template-dir*
              (io/file "leiningen/new/project")

              *project-dir*
              (io/file (project-dir template-data))]

      (info "Generating fresh project")
      (->dir "." (excludes feature-files features) template-data)

      )))
