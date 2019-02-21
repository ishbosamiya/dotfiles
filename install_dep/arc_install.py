import os

os.system("apt install php")
os.system("apt install php-curl")
os.system("apt install git")
print("Installed the dependencies")

print("Installing in home directory")
os.chdir(os.environ['HOME'])
os.system("mkdir .arcanist")
os.chdir(os.environ['HOME'] + "/.arcanist")
os.system("git clone https://github.com/phacility/libphutil.git")
os.system("git clone https://github.com/phacility/arcanist.git")
print("Cloned arcanist repositories")

os.system("export PATH=\"$PATH:"+os.environ['HOME']+"./arcanist/arcanist/bin/\"")
print("Added arc to path variables")
