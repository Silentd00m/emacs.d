(load "custom")

(defgroup gears nil
  "Awesome Emacs configuration"
  :group 'environment
  :link '(url-link :tag "Github" "https://github.com/Silentd00m/emacs.d"))

(defcustom gears-installed nil
  "If nil gears will resinstall."

  :type 'boolean
  :group 'gears)
