#include <iostream>
#include <cmath>
#include <utility>
#include <vector>
#include <map>
#include <algorithm> 
#include <limits>
#include <chrono> 
#include <unordered_map>
#include <fstream>
#include <numeric>   
#include <iomanip>   
#include <tuple>

using namespace std;

// Make structure for tree nodes so have label and isBlack bollean attached 
struct Node {
    pair<int, int> value;
    bool isBlack;
    Node(int first, int second = 0, bool black = false) : value(first, second), 
    isBlack(black) {}
};

// Overload the output operator for the Node structure
ostream& operator<<(ostream& os, const Node& node) {
    os << "(" << node.value.first << "," << node.value.second << ")";
    if (node.isBlack) {
        os << " Is Black";
    } else {
        os << " Is not Black";
    }
    return os;
}

// Function to build the binomial tree
vector<vector<Node>>* buildTree(int T) {
    vector<vector<Node>>* list = new vector<vector<Node>>;

    for (int t = 0; t <= T; ++t) {
        vector<Node> row;
        for (int j = 0; j <= t; ++j) {
            int value = j;
            row.push_back(Node(t, value));
        }
        list->push_back(row);
    }

    return list;
}

// Labelling the nodes black
// Generate all combinations of black and white for specified column using binary numbers
void generateCombs(vector<vector<Node>>* tree, int column, 
vector<vector<vector<Node>>>* labeledTrees) {
    int numNodes = tree->at(column).size();
    int totalCombinations = 1 << numNodes; // 2^numNodes in binary digits

    // Iterate through all possible combinations of black and white nodes in the column
    for (int i = 0; i < totalCombinations; ++i) {
        vector<vector<Node>> newTree = *tree;
        for (int j = 0; j < numNodes; ++j) {
            if (i & (1 << j)) {
                newTree[column][j].isBlack = true;
            } 
            else {
                newTree[column][j].isBlack = false;
            }
        }
        labeledTrees->push_back(newTree);
    }
}

// Label black nodes in the tree
void labelBlackNodes(vector<vector<Node>>* tree, int T, 
                    vector<vector<vector<Node>>>* labeledTrees) {
    // Generate combinations for the last column first
    generateCombs(tree, T - 1, labeledTrees);

    // Iterate through columns in reverse order (excluding the last one)
    for (int col = T - 2; col >= 0; --col) {
        vector<vector<vector<Node>>> tempList;
        generateCombs(tree, col, &tempList);
        vector<vector<vector<Node>>> updatedList;

        // Find combinations of trees w.r.t certain conditions
        for (int j = 0; j < tempList.size(); ++j) {
            for (int k = 0; k < labeledTrees->size(); ++k) {
                // Check the conditions for merging
                bool canMerge = true;
                if (tempList[j][col][0].isBlack && 
                        !labeledTrees->at(k)[col + 1][0].isBlack) {
                    canMerge = false;
                }
                if (tempList[j][col].back().isBlack && 
                        !labeledTrees->at(k)[col + 1].back().isBlack) {
                    canMerge = false;
                }
                for (int nodeIdx = 1; nodeIdx < tempList[j][col].size(); ++nodeIdx) { 
                    if (tempList[j][col][nodeIdx].isBlack && 
                        tempList[j][col][nodeIdx - 1].isBlack &&
                        !labeledTrees->at(k)[col + 1][nodeIdx].isBlack) {
                        canMerge = false;
                    }
                    if (tempList[j][col][nodeIdx].isBlack && 
                    tempList[j][col][nodeIdx + 1].isBlack && 
                    !labeledTrees->at(k)[col + 1][nodeIdx + 1].isBlack) {
                        canMerge = false;
                    }
                }
                if (canMerge) {
                    vector<vector<Node>> newTree = tempList[j];
                    for (int i = col + 1; i < T; ++i) {
                        newTree[i] = labeledTrees->at(k)[i];
                    }
                    updatedList.push_back(newTree);
                }
            }
        }
        *labeledTrees = updatedList;
    }
}

// Generate Returns from a given strategy 
vector<pair<double, double>> generateReturns(int T, vector<vector<Node>>* list, 
                                            double h, double probwin, double x) {
    // Generate the payouts list with markers
    vector<vector<tuple<double, double, bool>>> newList = {{{x, 1, false}}}; 

    // Mark the payoff as fixed or variable based on if the node is black or white 
    for (int i = 0; i < T; ++i) {
        vector<vector<tuple<double, double, bool>>> tempList(newList.size() + 1); 
        for (int j = 0; j < list->at(i).size(); ++j) {
            if (list->at(i)[j].isBlack) {
                for (int p = 0; p < newList[j].size(); ++p) {
                    get<2>(newList[j][p]) = true; 
                }
            }
        }

        for (int k = 0; k < newList.size(); ++k) {
            for (const auto& item : newList[k]) {
                if (get<2>(item)) { // If the marker is true, the value is fixed
                    tempList[k].push_back({get<0>(item), get<1>(item) * probwin, true}); 
                    tempList[k + 1].push_back({get<0>(item), get<1>(item) 
                        * (1 - probwin), true}); 
                } 
                else { // If the marker is false, add/subtract h specified 
                    tempList[k].push_back({get<0>(item) + h, get<1>(item) 
                            * probwin, false});
                    tempList[k + 1].push_back({get<0>(item) - h, get<1>(item) 
                            * (1 - probwin), false}); 
                }
            }
        }

        newList = tempList;        
    }

    // Get integer value from key pair to define new list
    vector<pair<double, double>> returnList;
    for (const auto& sublist : newList) {
        for (const auto& item : sublist) {
            returnList.push_back({get<0>(item), get<1>(item)});
        }
    }

    return returnList;
}

// Generate unique payoff list
pair<vector<double>, vector<double>> generateUniqueReturns(vector<pair<double, double>>* newList) {
    
    // Create a map to store the unique elements and their summed probabilities
    map<double, double> countMap;
    for (const auto& pair : *newList) {
        countMap[pair.first] += pair.second;
    }

    // Create the list of unique elements
    vector<double> uniqueList;
    vector<double> probabilities;
    for (const auto& pair : countMap) {
        uniqueList.push_back(pair.first);
        probabilities.push_back(pair.second);
    }

    return {uniqueList, probabilities};
}

// Calculate CPT value function v(x)
double valueFunction(double x, double alpha, double beta, double lambda) {
    if (x == 0) {
        return 0.0;
    } 
    else if (x > 0) {
        return pow(x, alpha);
    } 
    else {
        return -lambda * pow((-x), beta);
    }
}

// Function to calculate the probability weighting function w(P)
double weightingFunction(double P, double curvature_parameter) {
    // Handle special case where delta is very small
    const double epsilon = numeric_limits<double>::epsilon();
    double value_safe = max(curvature_parameter, epsilon);  
    
    // Tversky & Kahnenman (1992)
    return pow(P, value_safe) / pow(pow(P, value_safe) + 
                pow((1 - P), value_safe), (1.0 / value_safe));

    // Prelec (1998)
    //return exp(-pow(-log(P), value_safe));

    // Goldstein & Einhorn (1987)
    // double k = 0.77;
    //return k * pow(P, value_safe) / (k * pow(P, delta_safe) + pow(1 - P, value_safe));
}

// Function to calculate the CPT value 
double calculateCPTValue(double alpha, double beta, double lambda, double delta, 
                        double gamma, vector<double>* probList, vector<double>* uniqueReturnList) {
    double CPTValue = 0.0;

    // Create pairs of payoffs and probabilities
    vector<pair<double, double>> payoffProbPairs;
    for (int i = 0; i < uniqueReturnList->size(); ++i) {
        payoffProbPairs.emplace_back(uniqueReturnList->at(i), probList->at(i));
    }

    // Separate the pairs into negative and positive lists
    vector<pair<double, double>> negativePayoffProbPairs;
    vector<pair<double, double>> positivePayoffProbPairs;
    for (const auto& pair : payoffProbPairs) {
        if (pair.first < 0) {
            negativePayoffProbPairs.push_back(pair);
        } else {
            positivePayoffProbPairs.push_back(pair);
        }
    }

    // Sort both lists by payoffs in ascending order
    sort(negativePayoffProbPairs.begin(), negativePayoffProbPairs.end());
    sort(positivePayoffProbPairs.begin(), positivePayoffProbPairs.end());

    // Precompute cumulative probabilities for positive payoff pairs
    vector<double> positiveCumProbs(positivePayoffProbPairs.size() + 1, 0.0);
    for (int i = positivePayoffProbPairs.size() - 1; i >= 0; --i) {
        positiveCumProbs[i] = positiveCumProbs[i + 1] + positivePayoffProbPairs[i].second;
    }

    // Calculate CPT value for positive payoff pairs
    for (int i = 0; i < positivePayoffProbPairs.size(); ++i) {
        double cumProb1 = positiveCumProbs[i];
        double cumProb2 = positiveCumProbs[i + 1];
        double payoffValue = valueFunction(positivePayoffProbPairs[i].first, alpha, beta, lambda);

        if (i == positivePayoffProbPairs.size() - 1) {
            CPTValue += payoffValue * weightingFunction(cumProb1, delta);
        } else {
            CPTValue += payoffValue * (weightingFunction(cumProb1, delta) 
            - weightingFunction(cumProb2, delta));
        }
    }

    // Precompute cumulative probabilities for negative payoff pairs
    vector<double> negativeCumProbs(negativePayoffProbPairs.size() + 1, 0.0);
    for (int i = 0; i < negativePayoffProbPairs.size(); ++i) {
        negativeCumProbs[i + 1] = negativeCumProbs[i] + negativePayoffProbPairs[i].second;
    }

    // Calculate CPT value for negative payoff pairs
    for (int i = 0; i < negativePayoffProbPairs.size(); ++i) {
        double cumProb1 = negativeCumProbs[i + 1];
        double cumProb2 = negativeCumProbs[i];
        double payoffValue = valueFunction(negativePayoffProbPairs[i].first, alpha, beta, lambda);

        if (i == 0) {
            CPTValue += payoffValue * weightingFunction(cumProb1, gamma);
            // // For Prelec (1998) and Goldstein & Einhorn (1987)
            // CPTValue += payoffValue * weightingFunction(cumProb1, delta);
        } else {
            CPTValue += payoffValue * (weightingFunction(cumProb1, gamma) - weightingFunction(cumProb2, gamma));
            // For Prelec (1998) and Goldstein & Einhorn (1987)
            // CPTValue += payoffValue * (weightingFunction(cumProb1, gamma) - weightingFunction(cumProb2, gamma));
            
        }
    }

    return CPTValue;
}

// Function to find the optimal strategy based on maximising CPT value
void findOptimalStrategy(double currentValue, vector<vector<Node>>* currentTree, double* maxCPT, vector<vector<vector<Node>>>* optimalStrategies) {
    if (currentValue > *maxCPT) {
        *maxCPT = currentValue;
        optimalStrategies->clear();
        optimalStrategies->push_back(*currentTree); 
    } else if (currentValue == *maxCPT) { 
        optimalStrategies->push_back(*currentTree);
    }
}

// Function to convert the final column of the Casino Tree to black for printing purposes
void convertFinalColumnToBlack(vector<vector<vector<Node>>>* trees, int T) {
    for (auto& tree : *trees) {   
        for (int i = T; i > T - 1; --i) {
            for (int j = 0; j < tree[i].size(); ++j) {
                tree[i][j].isBlack = true;
            }
        }
    }
}

// Function to print the optimal strategy
void printOptimalTree(double optimalValue, vector<vector<vector<Node>>>* optimalStrategies, int T, bool Naive) {
    if (Naive) {
        cout << "The Optimal revised naive's agent strategy is represented in the following Casino Tree:" << endl;
    }
    else {
        cout << "At T = 0, the maximum CPT Value is: " << optimalValue << " given by the initial optimal strategy represented in the following Casino Tree:" << endl;
    }
    convertFinalColumnToBlack(optimalStrategies, T);
    for (auto& optimalTree : *optimalStrategies) {
        cout << "------------------" << endl;
        for (const auto& row : optimalTree) {
            for (const auto& node : row) {
                cout << node << " ";
            }
            cout << endl;
        }
        cout << "------------------" << endl;
    }
}

// Function to simulate Barberis's (2012) Casino Game
pair<double, vector<vector<vector<Node>>>> simulateCasinoGame(double alpha, double beta, double lambda, double delta, double gamma, double probW, double h, int T, double initialWealth) {
    // Parameters needed to find Optimal Strategy 
    double maxCPT = numeric_limits<double>::lowest();
    vector<vector<vector<Node>>> optimalStrategies;

    // Build the binomial tree
    vector<vector<Node>>* tree = buildTree(T);

    // Label nodes as black to get strategies
    vector<vector<vector<Node>>> labeledTrees;
    labelBlackNodes(tree, T, &labeledTrees);
    //cout << "Total number of combinations: " << labeledTrees.size() << endl;

    // Calculate CPT value for each strategy
    for (auto& tree : labeledTrees) {
        // Generate payoffs
        vector<pair<double, double>> returnList = generateReturns(T, &tree, h, probW, initialWealth);
        auto result = generateUniqueReturns(&returnList);
        vector<double> uniqueReturnList = result.first;
        vector<double> probList = result.second;

        // Calculate CPT values
        double CPTValues = calculateCPTValue(alpha, beta, lambda, delta, gamma, &probList, &uniqueReturnList);

        // Find max CPT value and hence optimal Strategy
        findOptimalStrategy(CPTValues, &tree, &maxCPT, &optimalStrategies);
    }


    // Clean up dynamically allocated memory
    delete tree;

    return make_pair(maxCPT, optimalStrategies);
}

// Function to find the current wealth of the agent at node (t, j)
void generateWealthList(vector<vector<double>>& wealthList,int T, double h) {
    for (int t = 0; t <= T; ++t) {
        vector<double> level;
        for (int j = 0; j <= t; ++j) {
            level.push_back(h * (t - 2 * j));
        }
        wealthList.push_back(level);
    }
}

// Function to catagorise gain-exit and loss-exit strategies 
void isGainGreaterThanLoss(pair<int, int>& strat, const vector<vector<vector<Node>>>& optimalStrategies) {
    double gainSum = 0.0;
    double lossSum = 0.0;

    // Iterate through the strategies and nodes
    for (int i = 0; i < optimalStrategies.size(); ++i) { 
        for (double j = 1; j < optimalStrategies[i].size() - 1; ++j) { 
            for (int k = 0; k < optimalStrategies[i][j].size(); ++k) {
                const Node& node = optimalStrategies[i][j][k];

                // Check if the node is a gain node or a loss node based on its indices
                if (node.isBlack) {
                    if (k < (j/2)) { // Gain node conditions
                        gainSum += 1;
                    } 
                    else if (k > (j/2)) { // Loss node conditions
                        lossSum += 1;
                    }
                }
                
            }
        }
    }

    // Compare the sum of gain nodes with the sum of loss nodes
    if (gainSum >= lossSum) {
        strat.first += 1;
        //cout << "Optimal Strategy is Gain-Exit" << endl;
    }
    else if (gainSum < lossSum) {
        strat.second += 1;
        //cout << "Optimal Strategy is Loss-Exit" << endl;
    }
}

// Function to measure computation time
void printComputationTime(chrono::time_point<chrono::high_resolution_clock> start,
                          chrono::time_point<chrono::high_resolution_clock> end) {
    auto duration = chrono::duration_cast<chrono::milliseconds>(end - start);
    cout << "Computation time: " << duration.count() << " milliseconds" << endl;
}

// Function to calculate the Naive agent's optimal strategy
pair<int, vector<vector<vector<Node>>>> naiveAgent(int& changes, vector<vector<vector<Node>>> optimalStrategies, double alpha, double beta, double lambda, double delta, double gamma, double probW, double h, int T) {
    // Create wealth list to find Current utility at node (t, j)
    vector<vector<double>> wealthList;
    generateWealthList(wealthList, T, h);

    // Find optimal strategy based on maximising CPT value
    vector<vector<Node>>* newStrategy = buildTree(T);
    for (int i = 1; i < T; ++i) {
        for (int j = 0; j < optimalStrategies[0][i].size(); ++j) {
            auto result = simulateCasinoGame(alpha, beta, lambda, delta, gamma, probW, h, T - i, wealthList[i][j]);
            double CPT = result.first;
            double currentUtility = valueFunction(wealthList[i][j], alpha, beta, lambda);
            if (CPT <= currentUtility) {
                newStrategy->at(i)[j].isBlack = true;
            }
        }
    }

    // Check if trees are different 
    bool foundDifference = false;
    for (int i = 0; i < optimalStrategies.size() && !foundDifference; ++i) {
        for (int j = 1; j < optimalStrategies[i].size() - 1 && !foundDifference; ++j) {
            for (int k = 0; k < optimalStrategies[i][j].size(); ++k) {
                if (optimalStrategies[i][j][k].isBlack != newStrategy->at(j)[k].isBlack) {
                    changes += 1;
                    foundDifference = true;
                    break;
                }
            }
        }
    }
    vector<vector<vector<Node>>> strategies{*newStrategy};        
    return make_pair(changes, strategies);

}

int main() {
    //Generate list of parameters Method 1
    int numElements = 20;
    vector<double> alphas(numElements);
    double start_alpha = 0.0, end_alpha = 1.0;

    // Initialise and scale alpha values between start_alpha and end_alpha
    iota(alphas.begin(), alphas.end(), 0);  
    transform(alphas.begin(), alphas.end(), alphas.begin(), [start_alpha, end_alpha, numElements](double i) {
        return start_alpha + i * (end_alpha - start_alpha) / (numElements - 1);
    });

    vector<double> deltas(numElements);
    double start_delta = 0.3, end_delta = 1.0;

    // Initialise and scale delta values between start_delta and end_delta
    iota(deltas.begin(), deltas.end(), 0);  
    transform(deltas.begin(), deltas.end(), deltas.begin(), [start_delta, end_delta, numElements](double i) {
        return start_delta + i * (end_delta - start_delta) / (numElements - 1);
    });

    vector<double> lambdas(numElements);
    double start_lambda = 1.0, end_lambda = 4.0;

    // Initialise and scale lambda values between start_lambda and end_lambda
    iota(lambdas.begin(), lambdas.end(), 0);  
    transform(lambdas.begin(), lambdas.end(), lambdas.begin(), [start_lambda, end_lambda, numElements](double i) {
        return start_lambda + i * (end_lambda - start_lambda) / (numElements - 1);
    });

    // Initialise parameters
    double probWin = 0.5;
    double h = 10;
    int T = 5;

    // Start measuring time
    auto start = chrono::high_resolution_clock::now();

    int changes = 0; // to find number of devations from intial strategy
    pair<int, int> strat = {0, 0};  // Initialise (gains, losses) for the CASA's strategy
    pair<int, int> stratNaive = {0, 0};  // Initialize (gains, losses) for the NA's strategy
    int m = 0; // to find number of valid triples
    vector<tuple<double, double, double, double, int, int>> values; // for exporting to file
    int tempGain = 0, tempLoss = 0, tempChange = 0;

    // Iterate through all combinations of alpha, lambda, and delta values
    for (double alpha : alphas) {
        for (double lambda : lambdas) {
            for (double delta : deltas) {
                // Simulate the casino game for the given parameters
                auto result = simulateCasinoGame(alpha, alpha, lambda, delta, delta, probWin, h, T, 0);

                if (result.first > 0) {
                    m += 1;

                    // Update strategies based on the simulation results
                    isGainGreaterThanLoss(strat, result.second);
                    auto resultNaive = naiveAgent(changes, result.second, alpha, alpha, lambda, delta, delta, probWin, h, T);
                    isGainGreaterThanLoss(stratNaive, resultNaive.second);

                    // Track and store the strategies 
                    if (strat.first > tempGain) {
                        values.push_back(make_tuple(alpha, lambda, delta, result.first, 1, changes > tempChange ? 1 : 0));
                    } else if (strat.second > tempLoss) {
                        values.push_back(make_tuple(alpha, lambda, delta, result.first, -1, changes > tempChange ? 1 : 0));
                    }
                    tempChange = changes, tempGain = strat.first, tempLoss = strat.second;
                }
            }   
        }
    }

    // Output results to the console
    cout << "Number of valid triples: " << m << "\n";
    cout << "Gain Strategy for T = 0: " << strat.first << ", Loss Strategy for T = 0: " << strat.second << "\n";
    cout << "Total number of Deviations from Original Strategy: " << changes << "\n";
    cout << "Gain Strategy for Naive Agent: " << stratNaive.first << ", Loss Strategy for Naive Agent: " << stratNaive.second << "\n";

    // Output results to the file
    ofstream outFile("Appendix11.7T&KT=5.txt", ios::out);
    outFile.close();
    outFile.open("Appendix11.7T&KT=5.txt");
    for (const auto& value : values) {
        outFile << get<0>(value) << " " << get<1>(value) << " " << get<2>(value) << " " << get<3>(value) << " " << get<4>(value) << " " << get<5>(value) << "\n";
    }
    outFile.close();

    // // Input validation loop using User Inputs Method 2
    // double alpha, beta, lambda, delta, gamma, h, probWin;
    // int T;

    // do {
    //     cout << "Enter alpha (0, 1): ";
    //     cin >> alpha;
    // } while (alpha <= 0 || alpha >= 1);

    // do {
    //     cout << "Enter beta (0, 1): ";
    //     cin >> beta;
    // } while (beta <= 0 || beta >= 1);

    // do {
    //     cout << "Enter lambda (>= 1): ";
    //     cin >> lambda;
    // } while (lambda < 1);

    // do {
    //     cout << "Enter delta (0, 1): ";
    //     cin >> delta;
    // } while (delta <= 0 || delta >= 1);

    // do {
    //     cout << "Enter Gamma (0, 1): ";
    //     cin >> gamma;
    // } while (gamma <= 0 || gamma >= 1);

    // do {
    //     cout << "Enter h: ";
    //     cin >> h;
    // } while (cin.fail() || h <= 0); 

    // do {
    //     cout << "Enter Win Probability (0,1): ";
    //     cin >> probWin;
    // } while (probWin <= 0 || probWin >= 1);  

    // do {
    //     cout << "Enter T: ";
    //     cin >> T;
    // } while (cin.fail() || T <= 0);  


    // cout << "Inputs accepted: alpha = " << alpha << ", beta = " << beta << ", lambda = " << lambda
    //      << ", delta = " << delta << ", Gamma = " << gamma << ", h = " << h << ", Win probability = " << probWin << ", T = " << T << endl;

    // auto start = chrono::high_resolution_clock::now();

    // int changes = 0;
    // pair<int, int> strat = {0, 0};  // (gains, losses);
    // pair<int, int> stratNaive = {0, 0};  // (gains, losses);

    // auto result = simulateCasinoGame(alpha, beta, lambda, delta, gamma, probWin, h, T, 0);
    // if (result.first > 0) {
    //     double optCPT = result.first;
    //     vector<vector<vector<Node>>> optimalStrategies = result.second;
    //     cout << "------------------" << endl;
    //     cout << "------------------" << endl;
    //     printOptimalTree(optCPT, &optimalStrategies, T, false);
    //     isGainGreaterThanLoss(strat, result.second);


    //     // Naive Agent
    //     auto resultNaive = naiveAgent(changes, result.second, alpha, alpha, lambda, delta, delta, probWin, h, T);
    //     vector<vector<vector<Node>>> optimalStrategiesNaive = resultNaive.second;
    //     cout << "------------------" << endl;
    //     cout << "------------------" << endl;
    //     printOptimalTree(0, &optimalStrategiesNaive, T, true);
    //     isGainGreaterThanLoss(stratNaive, resultNaive.second);
    // }

    // Stop measuring time
    auto end = chrono::high_resolution_clock::now();
    printComputationTime(start, end);

    return 0;
}