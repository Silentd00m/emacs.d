(defgroup gears-layers/mail nil
  "Mail Layer Configuration"

  :group 'gears-layers)

(defcustom gears-layers/mail-imap-handler 'elmo
  "Which mail handler to use for fetching mail. offlineimap is faster but has external dependencies"

  :type '(choice (const elmo)
                 (const offlineimap)))

(defcustom gears-layers/mail-imap-accounts ()
  "List of all IMAP accounts."

  :type '(alist
          :tag "IMAP Accounts"
          :key-type (string :tag "Name")
          :value-type
          (set
           :format "%v"
           :entry-format "%b %v"
           (cons :format "%v"
                 (const :format "" server)
                 (string :tag "Server"))
           (cons :format "%v"
                 (const :format "" port)
                 (integer :tag "Port"))
           (cons :format "%v"
                 (const :format "" username)
                 (string :tag "Username"))
           (cons :format "%v"
                 (const :format "" auth-type)
                 (choice :tag "Auth-Type"
                         (const login)
                         (const clear)
                         (const cram-md5)
                         (const digest-md5)))
           (cons :format "%v"
                 (const :format "" conn-type)
                 (choice :tag "Conn-Type"
                         (const clear)
                         (const ssl)
                         (const starttls)))))
  :set (lambda (symbol value))
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-smtp-accounts ()
  "List of available SMTP accounts."

  :type '(alist
          :tag "SMTP Accounts"
          :key-type (string :tag "Name")
          :value-type
          (set
           :format "%v"
           :entry-format "%b %v"
           (cons :format "%v"
                 (const :format "" wl-from)
                 (string :tag "From (Name <user1@example.com>)"))
           (cons :format "%v"
                 (const :format "" wl-smtp-posting-server)
                 (string :tag "Server"))
           (cons :format "%v"
                 (const :format "" wl-smtp-posting-port)
                 (integer :tag "Port"))
           (cons :format "%v"
                 (const :format "" wl-smtp-posting-user)
                 (string :tag "Username"))
           (cons :format "%v"
                 (const :format "" wl-smtp-authenticate-type)
                 (choice :tag "Auth-Type"
                         (const login)
                         (const clear)
                         (const cram-md5)
                         (const digest-md5)))
           (cons :format "%v"
                 (const :format "" wl-smtp-connection-type)
                 (choice :tag "Conn-Type"
                         (const clear)
                         (const ssl)
                         (const starttls)))
           (cons :format "%v"
                 (const :format "" wl-local-domain)
                 (string :tag "Domain (Mostly Server Domain)"))))
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-default-smtp-account (car (car gears-layers/mail-smtp-accounts))
  "Default account for smtp."

  :type 'string
  ;; :options (mapcar (lambda (acc)
  ;;                    (prin1-to-string (car acc)))
  ;;                  gears-layers/mail-smtp-accounts)
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-folders-path "~/.mail"
  "Path to where the downloaded Mail is stored."

  :type 'directory
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-draft-folder ".drafts"
  "Default draft storage folder."

  :type 'string
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-trash-folder ".trash"
  "Default trash folder."

  :type 'string
  :group 'gears-layers/mail)

(defcustom gears-layers/mail-queue-folder ".queue"
  "Default queue folder."

  :type 'string
  :group 'gears-layers/mail)

(defun gears-layers/mail-init ()
  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

  (setq elmo-imap4-use-modified-utf7 t
        wl-template-alist gears-layers/mail-smtp-accounts
        elmo-maildir-folder-path gears-layers/mail-folders-path
        elmo-localdir-folder-path gears-layers/mail-folders-path
        wl-stay-folder-window t
        wl-folders-file gears-layers/mail-folders-file
        wl-user-mail-address-list `,(mapcar #'(lambda (elem)
                                                (cdr (assoc 'wl-local-domain (cdr elem))))
                                            gears-layers/mail-smtp-accounts)
        wl-default-folder gears-layers/mail-folders-path
        wl-quicksearch-folder gears-layers/mail-folders-path
        wl-draft-folder gears-layers/mail-draft-folder
        wl-trash-folder gears-layers/mail-trash-folder
        wl-queue-folder gears-layers/mail-queue-folder
        wl-fcc-force-as-read t
        wl-from (cdr (assoc 'wl-from
                            (assoc gears-layers/mail-default-smtp-account
                                   gears-layers/mail-smtp-accounts)))))

(defcustom gears-layers/mail-folders-file (s-concat gears-emacs-basepath
                                                    "/config/mail-folders.conf")
  "Location of the IMAP folders file."

  :type 'string)

(defun gears-layers/mail-description ()
  "Returns the description of the company layer."

  "Easy to use mail client configuration.")

(defun gears-layers/mail-install ()
  t)

(defun gears-layers/mail-configure (httpcon)
  "Mail layer configuration UI."

  "OK")

(defun gears-layers/mail-remove ()
  t)

(gears-layer-defdepends mail
                        :packages '(wanderlust offlineimap))
