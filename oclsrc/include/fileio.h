
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

const string filename = "data/Solution.dat";

float* READ(const char *token) {
    cout << "Reading token " << token << " ..." << endl;
    ifstream dfile(filename);
    string dline;
    if (dfile.is_open()) {
        while (getline(dfile, dline)) {
            if (dline.find(token) != string::npos)
                break;
        }
        dfile.close();
    }

    int pos = dline.find(":") + 1;
    
    string dstr = dline.substr(pos, dline.length()-pos);
    istringstream iss(dstr); 
    
    vector<float> res((istream_iterator<double>(iss)), 
                       istream_iterator<double>());

    float * ptr = (float*) malloc(sizeof(float)*res.size());

    copy(res.begin(), res.end(), ptr);

    return ptr;
}
