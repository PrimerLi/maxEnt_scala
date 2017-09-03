def readFile(fileName):
    import os
    import sys
    if (not os.path.exists(fileName)):
        print fileName + " does not exist. "
        sys.exit(-1)

    x = []
    y = []
    ifile = open(fileName, "r")
    for (index, string) in enumerate(ifile):
        a = string.split("  ")
        x.append(float(a[0]))
        y.append(float(a[1]))
    ifile.close()
    return x, y

def printFile(x, y, outputFile):
    assert(len(x) == len(y))
    ofile = open(outputFile, "w")
    for i in range(len(x)):
        ofile.write(str(x[i]) + "  " + str(y[i]) + "\n")
    ofile.close()

def main():
    import os
    import sys

    x, y = readFile("model.txt")
    s = 0.0
    dx = x[1] - x[0]
    for i in range(len(x)-1):
        s = s + 0.5*(y[i] + y[i+1])*dx
    for i in range(len(y)):
        y[i] = y[i]/s
    printFile(x, y, "model_normalized.txt")
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
