(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(flyspell-default-dictionary "english")
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(impatient-mode simple-httpd rg flycheck yasnippet cmake-mode restart-emacs lsp-python-ms magit general dap-mode fold-this dash lsp-ui lsp-mode iedit sourcetrail projectile ido-completing-read+ flx-ido amx which-key clang-format+ olivetti unfill centered-window cargo rust-mode arduino-mode scad-preview scad-mode pdf-tools ag glsl-mode smex ess ggtags writegood-mode org company company-c-headers))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "cd ../../rusted-keys/.dep/egui_glfw/ && cargo run --example simple_gui")
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
