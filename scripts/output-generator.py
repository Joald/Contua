import os

for file_name in os.listdir("good"):
    if file_name != "library.cont":
        os.system("stack run good/library.cont good/%s 2>/dev/null >goodout/%s" % (file_name, file_name.replace(".cont", ".out")))

for file_name in os.listdir("bad"):
    os.system("stack run bad/%s 2>/dev/null >badout/%s" % (file_name, file_name.replace(".cont", ".out")))
