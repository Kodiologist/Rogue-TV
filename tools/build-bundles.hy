(import
  os
  importlib
  [subprocess [check-call check-output]]
  datetime)

(unless (os.path.exists "build")
  (os.mkdir "build"))
(setv target-posix "build/roguetv-posix.tar.gz")
(setv target-windows "build/roguetv-windows.zip")

; Create the VERSION files.
(setv commit (.rstrip (.decode
  (check-output ["git" "log" "-1" "--format=%H"]) "ASCII")))
(setv timestamp (.isoformat (.replace (datetime.datetime.utcnow) :microsecond 0)))
(for [platform ["posix" "windows"]]
  (with [o (open (.format "bundle-{}/VERSION" platform) "w")] (o.write (+
    platform "\n"
    "Git commit " commit "\n"
    "Packaged " timestamp "\n"))))

; Refresh symbolic links.
(for [mname ["hy" "appdirs" "inflect" "jsonpickle" "pypaths" "rply" "clint"]]
  (setv m (importlib.import-module mname))
  (setv source m.__file__)
  (setv dest (os.path.join "bundle-posix/lib" mname))
  (if (= (os.path.basename source) "__init__.py")
    ; This module has a directory tree of files.
    (setv source (os.path.dirname source))
    ; Otherwise, it's just a single file.
    (+= dest ".py"))
  (when (os.path.lexists dest)
    (os.remove dest))
  (os.symlink source dest)
  (unless (os.path.exists dest)
    (exit (+ "Broken symbolic link?: " dest))))

(check-call ["tar"
  "-h" "--posix" "--numeric-owner"
  "--exclude" "__pycache__"
  "--transform" "s!^[^/]+!roguetv-posix!x"
  "-zcf" target-posix "bundle-posix"])

(check-call ["zip"
  "-r" "--quiet"
  target-windows "bundle-windows"])
