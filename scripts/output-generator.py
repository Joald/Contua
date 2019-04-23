import os

for file_name in os.listdir("good"):
    os.system("stack run good/%s 2>/dev/null >goodout/%s" % (file_name, file_name.replace(".cont", ".out")))

for file_name in os.listdir("bad"):
    os.system("stack run bad/%s 2>/dev/null >badout/%s" % (file_name, file_name.replace(".cont", ".out")))
