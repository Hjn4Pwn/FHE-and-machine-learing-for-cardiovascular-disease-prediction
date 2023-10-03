#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>
#include <map>
#include <algorithm>
#include <random>
#include <cstdio>
#include <chrono>
#include "openfhe.h"
#define PROFILE

using namespace lbcrypto;
using namespace std;

class Table {
	public:
		vector<string> attrName;
		vector<vector<string> > data;

		vector<vector<string> > attrValueList;
		void extractAttrValue() {
			attrValueList.resize(static_cast<int>(attrName.size()));
			for(int j=0; j<static_cast<int>(attrName.size()); j++) {
				map<string, int> value;
				for(int i=0; i<static_cast<int>(data.size()); i++) {
					value[data[i][j]]=1;
				}

				for(auto iter=value.begin(); iter != value.end(); iter++) {
					attrValueList[j].push_back(iter->first);
				}
			}
		}
};

class Node {
	public:
		int criteriaAttrIndex=-1;
		string attrValue;

		int treeIndex=-1;
		bool isLeaf;
		string label;

		vector<int > children;

		Node() {
			isLeaf = false;
		}
};

class DecisionTree {
	public:
		Table initialTable;
		vector<Node> tree;

		DecisionTree(Table table) {
			initialTable = table;
			initialTable.extractAttrValue();

			Node root;
			root.treeIndex=0;
			tree.push_back(root);
			run(initialTable, 0);
			printTree(0, "");

			cout<< "<-- finish generating decision tree -->" << endl << endl;
		}

		string guess(vector<string> row) {
			string label = "";
			int leafNode = dfs(row, 0);
			if(leafNode == -1) {
				return "dfs failed";
			}
			label = tree[leafNode].label;
			return label;
		}

		int dfs(vector<string>& row, int here) {
			if(tree[here].isLeaf) {
				return here;
			}

			int criteriaAttrIndex = tree[here].criteriaAttrIndex;

			for(int i=0;i<static_cast<int>(tree[here].children.size()); i++) {
				int next = tree[here].children[i];

				if (row[criteriaAttrIndex] == tree[next].attrValue) {
					return dfs(row, next);
				}
			}
			return -1;
		}

		void run(Table table, int nodeIndex) {
			if(isLeafNode(table) == true) {
				tree[nodeIndex].isLeaf = true;
				tree[nodeIndex].label = table.data.back().back();
				return;
			}

			int selectedAttrIndex = getSelectedAttribute(table);

			map<string, vector<int> > attrValueMap;
			for(int i=0;i<static_cast<int>(table.data.size());i++) {
				attrValueMap[table.data[i][selectedAttrIndex]].push_back(i);
			}

			tree[nodeIndex].criteriaAttrIndex = selectedAttrIndex;

			pair<string, int> majority = getMajorityLabel(table);
			if((double)majority.second/table.data.size() > 0.8) {
				tree[nodeIndex].isLeaf = true;
				tree[nodeIndex].label = majority.first;
				return;
			}

			for(int i=0;i< static_cast<int>(initialTable.attrValueList[selectedAttrIndex].size()); i++) {
				string attrValue = initialTable.attrValueList[selectedAttrIndex][i];

				Table nextTable;
				vector<int> candi = attrValueMap[attrValue];
				for(int i=0;i<static_cast<int>(candi.size()); i++) {
					nextTable.data.push_back(table.data[candi[i]]);
				}

				Node nextNode;
				nextNode.attrValue = attrValue;
				nextNode.treeIndex = (int)tree.size();
				tree[nodeIndex].children.push_back(nextNode.treeIndex);
				tree.push_back(nextNode);

				// for empty table
				if(static_cast<int>(nextTable.data.size())==0) {
					nextNode.isLeaf = true;
					nextNode.label = getMajorityLabel(table).first;
					tree[nextNode.treeIndex] = nextNode;
				} else {
					run(nextTable, nextNode.treeIndex);
				}
			}
		}

		double getEstimatedError(double f, int N) {
			double z = 0.69;
			if(N==0) {
				cout << ":: getEstimatedError :: N is zero" << endl;
				exit(0);
			}
			return (f+z*z/(2*N)+z*sqrt(f/N-f*f/N+z*z/(4*N*N)))/(1+z*z/N);
		}

		pair<string, int> getMajorityLabel(Table table) {
			string majorLabel = "";
			int majorCount = 0;

			map<string, int> labelCount;
			for(int i=0;i< static_cast<int>(table.data.size()); i++) {
				labelCount[table.data[i].back()]++;

				if(labelCount[table.data[i].back()] > majorCount) {
					majorCount = labelCount[table.data[i].back()];
					majorLabel = table.data[i].back();
				}
			}

			return {majorLabel, majorCount};
		}


		bool isLeafNode(Table table) {
			for(int i=1;i < static_cast<int>(table.data.size());i++) {
				if(table.data[0].back() != table.data[i].back()) {
					return false;
				}
			}
			return true;
		}

		int getSelectedAttribute(Table table) {
			int maxAttrIndex = -1;
			double maxAttrValue = 0.0;

			// except label
			for(int i=0; i< static_cast<int>(initialTable.attrName.size())-1; i++) {
				if(maxAttrValue < getGainRatio(table, i)) {
					maxAttrValue = getGainRatio(table, i);
					maxAttrIndex = i;
				}
			}

			return maxAttrIndex;
		}

		double getGainRatio(Table table, int attrIndex) {
			return getGain(table, attrIndex)/getSplitInfoAttrD(table, attrIndex);
		}

		double getInfoD(Table table) {
			double ret = 0.0;

			int itemCount = (int)table.data.size();
			map<string, int> labelCount;

			for(int i=0;i<static_cast<int>(table.data.size());i++) {
				labelCount[table.data[i].back()]++;
			}

			for(auto iter=labelCount.begin(); iter != labelCount.end(); iter++) {
				double p = (double)iter->second/itemCount;

				ret += -1.0 * p * log(p)/log(2);
			}

			return ret;
		}

		double getInfoAttrD(Table table, int attrIndex) {
			double ret = 0.0;
			int itemCount = (int)table.data.size();

			map<string, vector<int> > attrValueMap;
			for(int i=0;i<static_cast<int>(table.data.size());i++) {
				attrValueMap[table.data[i][attrIndex]].push_back(i);
			}

			for(auto iter=attrValueMap.begin(); iter != attrValueMap.end(); iter++) {
				Table nextTable;
				for(int i=0;i<static_cast<int>(iter->second.size()); i++) {
					nextTable.data.push_back(table.data[iter->second[i]]);
				}
				int nextItemCount = (int)nextTable.data.size();

				ret += (double)nextItemCount/itemCount * getInfoD(nextTable);
			}

			return ret;
		}

		double getGain(Table table, int attrIndex) {
			return getInfoD(table)-getInfoAttrD(table, attrIndex);
		}

		double getSplitInfoAttrD(Table table, int attrIndex) {
			double ret = 0.0;

			int itemCount = (int)table.data.size();

			map<string, vector<int> > attrValueMap;
			for(int i=0;i<static_cast<int>(table.data.size());i++) {
				attrValueMap[table.data[i][attrIndex]].push_back(i);
			}

			for(auto iter=attrValueMap.begin(); iter != attrValueMap.end(); iter++) {
				Table nextTable;
				for(int i=0;i<static_cast<int>(iter->second.size()); i++) {
					nextTable.data.push_back(table.data[iter->second[i]]);
				}
				int nextItemCount = (int)nextTable.data.size();

				double d = (double)nextItemCount/itemCount;
				ret += -1.0 * d * log(d) / log(2);
			}

			return ret;
		}

		
		void printTree(int nodeIndex, string branch) {
			if (tree[nodeIndex].isLeaf == true)
				cout << branch << "Label: " << tree[nodeIndex].label << "\n";

			for(int i = 0; i < static_cast<int>(tree[nodeIndex].children.size()); i++) {
				int childIndex = tree[nodeIndex].children[i];

				string attributeName = initialTable.attrName[tree[nodeIndex].criteriaAttrIndex];
				string attributeValue = tree[childIndex].attrValue;

				printTree(childIndex, branch + attributeName + " = " + attributeValue + ", ");
			}
		}
};


CCParams<CryptoContextBGVRNS> parametersBGV;
CryptoContext<DCRTPoly> cryptoContextBGV;
KeyPair<DCRTPoly> keyPairBGV;

Ciphertext<DCRTPoly> encBGV(int64_t x)
{
	vector<int64_t> r;
	r.push_back(x);
	Plaintext ptxt = cryptoContextBGV->MakePackedPlaintext(r);
	auto c = cryptoContextBGV->Encrypt(keyPairBGV.publicKey, ptxt);
	r.clear();
	return c;
	
}
vector<string> splitString(const string& s, char delimiter) {
    vector<string> tokens;
    stringstream ss(s);
    string token;
    while (getline(ss, token, delimiter)) {
        tokens.push_back(token);
    }
    return tokens;
}

vector<vector<string>> data_String;  
vector<char> columnLabels;  
map<string, Ciphertext<DCRTPoly>> labelValueMap;
char labelChar = 'A';
vector<vector<int64_t>> data_int;
vector<string> label;
string line;
bool isAttrName = true;

class InputReader {
	private:
		ifstream file;
		Table table;
	public:
		InputReader(string filename) {
			
			file.open(filename);
			if (file.is_open()) {
				if (getline(file, line)) {
					label = splitString(line, ',');
					table.attrName=label;
					isAttrName = false;
				}

				while (getline(file, line)) {
					vector<string> row = splitString(line, ',');
					vector<int64_t> numericRow;
					for (const auto& element : row) {
						numericRow.push_back(stod(element));
					}
					data_int.push_back(numericRow);
				}

				file.close();
			} else {
				cout << "Failed to open the file." << endl;
				exit(0);
			}

			
			for (int i = 0; i < static_cast<int>(label.size()); i++) {
				columnLabels.push_back(labelChar++);
			}

			for (int i = 0; i < static_cast<int64_t>(data_int.size()); i++) {
				vector<int64_t> row= data_int[i];
				vector<int64_t> row_not_enc = data_int[i];
				vector<Ciphertext<DCRTPoly>> row_enc;
				Ciphertext<DCRTPoly> row_enc_i;
				for (auto& row_i : row) {
					row_enc_i= encBGV(static_cast<int64_t>(row_i));
					row_enc.push_back(row_enc_i);
				}
				vector<string> labeledRow;

				for (int j = 0; j < static_cast<int>(row_not_enc.size()); j++) {
					string labeledElement = string(1, columnLabels[j]) + to_string(static_cast<int>(row_not_enc[j]));
					labeledRow.push_back(labeledElement);
					labelValueMap[labeledElement] = row_enc[j];  
				}

				data_String.push_back(labeledRow);
			}


			table.data = data_String;
		}
		Table getTable() {
			data_String.clear();
			data_int.clear();
			return table ;

		}
};
vector<Ciphertext<DCRTPoly>> res;
class OutputPrinter {
	private:
		ofstream fout;
	public:
		OutputPrinter(string filename) {
			fout.open(filename,ios::app);
			if(!fout) {
				cout << filename << " file could not be opened\n";
				exit(0);
			}
		}

		string joinByTab(vector<string> row) {
			for (const auto& labelValue : labelValueMap) {
			if (labelValue.first==row[static_cast<int>(row.size()-1)])
			{
				res.push_back(labelValue.second);
			}
   	 	}
			return row[static_cast<int>(row.size()-1)];
		}
		void addLine(string str) {
			fout << str << endl;
		}
};

string testFileName = "dt_test.txt";
string resultFileName = "dt_result.txt";
bool flag= true;
int main() {
		
	auto start = std::chrono::high_resolution_clock::now();

    	parametersBGV.SetMultiplicativeDepth(8);
    	parametersBGV.SetPlaintextModulus(65537);
    	parametersBGV.SetBatchSize(1);
    	cryptoContextBGV = GenCryptoContext(parametersBGV);
    	std::cout << "BGV scheme is using ring dimension " << cryptoContextBGV->GetRingDimension() << std::endl << std::endl;

    	cryptoContextBGV->Enable(PKE);
    	cryptoContextBGV->Enable(KEYSWITCH);
    	cryptoContextBGV->Enable(LEVELEDSHE);
    	keyPairBGV = cryptoContextBGV->KeyGen();


	std::cout << "Public key BGV: " << keyPairBGV.publicKey << std::endl;
	std::cout << "Secret key BGV: " << keyPairBGV.secretKey << std::endl;
	
	//remove(filename);
	InputReader testInputReader(testFileName);
	Table test = testInputReader.getTable();
	
	OutputPrinter outputPrinter(resultFileName);

	for (int trainIndex = 1; trainIndex <= 10; trainIndex++) 
	{
		cout<<"TRAIN: "<<trainIndex<<endl;
		string trainFileName = "dt_train" + to_string(trainIndex) + ".txt";
		InputReader trainInputReader(trainFileName);
		DecisionTree decisionTree(trainInputReader.getTable());

		for(int i=0;i < static_cast<int>(test.data.size()); i++) {
			vector<string> result = test.data[i];
			result.push_back(decisionTree.guess(test.data[i]));
			outputPrinter.addLine(outputPrinter.joinByTab(result));
		}
		labelValueMap.clear();
		
    	}
    	
    	
	
	Plaintext result;
	Ciphertext<DCRTPoly> avg = res[0];
	std::vector<int64_t> finalResult;
	if (static_cast<int>(res.size()) != 0) {
	    for (int i = 1; i < static_cast<int>(res.size()); i++) {
		avg = cryptoContextBGV->EvalAdd(avg, res[i]);
	    }
	    cryptoContextBGV->Decrypt(keyPairBGV.secretKey, avg, &result);
	    result->SetLength(8);
	    finalResult = result->GetPackedValue();
	    std::cout << "Risk of heart disease with Random Forest_BGV scheme: " << double(finalResult[0])/ static_cast<int>(res.size()) << std::endl;
	} else {
	    std::cout << "res.size()==0...." << std::endl;
	}


	auto end = std::chrono::high_resolution_clock::now();
	std::chrono::duration<double> duration = end - start;
    	double execution_time = duration.count();

    	std::cout << "Run time: " << execution_time << "s" << std::endl;
	return 0;
}
