#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib> // For system calls

using namespace std;

int main(int argc, char* argv[]) {
    if (argc != 2) {
        cout << "Usage: abapcheck filename.abap" << endl;
        return 1;
    }

    string filename = argv[1];
    ifstream abapFile(filename);
    if (abapFile.fail()) {
        cout << "Error while reading file " << filename << endl;
        return 1;
    }

    // Check if abaplint is installed
    if (system("abaplint --version") != 0) {
        cout << "Error: abaplint is not installed. Please install it using 'npm install -g abaplint'." << endl;
        return 1;
    }

    // Run abaplint on the file
    string command = "abaplint " + filename + " --format json > abaplint_output.json";
    int result = system(command.c_str());

    if (result != 0) {
        cout << "Syntax errors found in " << filename << ". Check abaplint_output.json for details." << endl;
        return 1;
    }

    cout << "No syntax errors found in " << filename << "." << endl;
    return 0;
}