(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(tango-dark))
 '(flyspell-default-dictionary "english")
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("/media/ish/data/extra/csgo_demolyzer/csgo_demolyzer_todo.org" "/media/ish/data/extra/struct_finder/struct_finder_notes.org"))
 '(package-selected-packages
   '(eat zoxide dtrt-indent sourcepawn-mode undo-fu-session lsp-java gnu-elpa-keyring-update lua-mode framemove go-mode typescript-mode csharp-mode default-text-scale winner-mode lsp-pyright yaml-mode nix-repl nix-mode command-log-mode fzf esup hl-todo hl-block-mode expand-region string-inflection impatient-mode simple-httpd rg flycheck yasnippet cmake-mode restart-emacs lsp-python-ms magit general dap-mode fold-this dash lsp-ui lsp-mode iedit sourcetrail projectile ido-completing-read+ flx-ido amx which-key clang-format+ olivetti unfill centered-window cargo rust-mode arduino-mode scad-preview scad-mode pdf-tools ag glsl-mode smex ess ggtags writegood-mode org company company-c-headers))
 '(safe-local-variable-values
   '((eval setenv "GO111MODULE" "on")
     (eval setenv "LD_LIBRARY_PATH"
           (let
               ((ld_library_path
                 (getenv "LD_LIBRARY_PATH"))
                (to_add "/media/ish/data/extra/csgo_demolyzer/deps/shaderc/lib"))
             (if ld_library_path
                 (format "%s:%s" ld_library_path to_add)
               to_add)))
     (eval setenv "SHADERC_LIB_DIR" "/media/ish/data/extra/csgo_demolyzer/deps/shaderc/lib")
     (projectile-project-test-cmd . "cd gpu/gpu_switcheroo && cargo test")
     (projectile-project-run-cmd . "cd gpu/gpu_switcheroo && cargo run")
     (projectile-project-compilation-cmd . "cd gpu/gpu_switcheroo && cargo build")
     (projectile-project-run-cmd . "cargo expand")
     (projectile-project-test-cmd . "cd builder && cargo test")
     (projectile-project-run-cmd . "cargo run")
     (projectile-project-compilation-cmd . "cargo build")
     (projectile-project-run-cmd . "cd ../../rusted-keys/.dep/egui_glfw/ && cargo run --example simple_gui")
     (projectile-project-run-cmd . "mold -run cargo run")
     (projectile-project-compilation-cmd . "mold -run cargo build")
     (eval progn
           (dap-register-debug-template "Blender Debug"
                                        (list :type "gdb" :request "launch" :name "Blender Debug" :target "${workspaceFolder}/../build_linux_debug/bin/blender" :cwd nil)))
     (projectile-project-test-cmd . "../build_linux_debug/bin/tests/bke_cloth_remesh_test --gtest_filter=\"cloth_remesh.*\"")
     (projectile-project-test-cmd . "../build_linux_debug/bin/tests/blender_test --gtest_filter=\"cloth_remesh.*\"")
     (projectile-project-test-cmd . "../build_linux_debug/bin/tests/blenlib_test --gtest_filter=\"generational_arena.*\"")
     (projectile-project-run-cmd . "../build_linux_debug/bin/blender")
     (projectile-project-compilation-cmd . "make debug"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
