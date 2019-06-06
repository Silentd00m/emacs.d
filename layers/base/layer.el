(gears-layer-defdepends base
                        :packages #'(lambda (&layers)
                                      `(bug-hunter
                                        diminish
                                        undo-tree
                                        smartparens
                                        comment-dwim-2
                                        rainbow-delimiters
                                        hideshowvis
                                        ,(when (version<= emacs-version "26.0")
                                           nlinum
                                           nlinum-hl)
                                        multiple-cursors
                                        powerline
                                        material-theme
                                        tabbar-ruler
                                        saveplace
                                        git
                                        magit
                                        visual-regexp
                                        origami
                                        hydra
                                        projectile
                                        projectile-speedbar
                                        helm
                                        helm-make
                                        helm-projectile
                                        helm-flycheck
                                        helm-flx
                                        helm-descbinds
                                        which-key
                                        treemacs
                                        stickyfunc-enhance
                                        w3m
                                        fill-column-indicator
                                        sublimity
                                        switch-buffer-functions
                                        zoom
                                        yascroll))
                        :layers #'(lambda (&layers)
                                    '(auto_completion)))
