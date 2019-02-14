import os

os.system("apt install cmake")
os.system("apt install libclang-dev")
os.system("apt install cmake")
os.system("apt install git")
print("Installed the dependencies")

print("Installing in home directory")
os.chdir(os.environ['HOME'])
os.system("git clone --recursive https://github.com/Andersbakken/rtags.git")
print("Cloned https://github.com/Andersbakken/rtags.git repository")
os.chdir(os.environ['HOME'] + "/rtags")
os.system("cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .")
print("cmake has finished")
os.system("make")
print("rtags has been installed")

emacs = input("\n\n\n\nIs emacs installed? (y or n): ")
if emacs == "y":
    print("Install cmake-ide")
    print("Add following to emacs init file\n")
    print(";; cmake-ide setup")
    print("(require 'rtags)")
    print("(cmake-ide-setup)")
    print("(setq rtags-path \"" + os.environ['HOME'] + "/rtags/bin\")")
    print("(add-hook 'c-mode-hook 'rtags-start-process-unless-running) ;; starting rdm (rtags daemon)")
    print("(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)")
    print("(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)")
elif emacs == "n":
    print("Would highly recommend it, please install emacs and get back :D")
else:
    while emacs != "y" and emacs != "n":
        emacs = input("Is emacs installed? (y or n): ")
    if emacs == "y":
        print("Install cmake-ide")
        print("Add following to emacs init file\n")
        print(";; cmake-ide setup")
        print("(require 'rtags)")
        print("(cmake-ide-setup)")
        print("(setq rtags-path \"" + os.environ['HOME'] + "/rtags/bin\")")
        print("(add-hook 'c-mode-hook 'rtags-start-process-unless-running) ;; starting rdm (rtags daemon)")
        print("(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)")
        print("(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)")
    elif emacs == "n":
        print("Would highly recommend it, please install emacs and get back :D")
