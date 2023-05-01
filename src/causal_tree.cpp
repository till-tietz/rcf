#include <iostream>
#include <vector>
#include <algorithm>
#include <math.h>
#include <tuple>
#include <cfloat>

using namespace std;

typedef std::pair<std::tuple<std::vector<std::vector<double>>,std::vector<double>,std::vector<int>>, std::tuple<std::vector<std::vector<double>>,std::vector<double>,std::vector<int>>> data_split;

// Node structue
struct Node {
    int depth;        
    int feature;      
    double threshold; 
    double value;     
    Node *left;       
    Node *right;      
};

// Function to compute ATE 
double tau(
    std::vector<double> &vec, 
    std::vector<int>& treat) {
    double mean_treat = 0.0;
    double mean_control = 0.0;
    int n_treat = 0;
    int n_control = 0;
    for(int i = 0; i < vec.size(); ++i) {
        if(treat[i] == 1) {
            mean_treat += vec[i];
            n_treat += 1;
        } else {
            mean_control += vec[i];
            n_control += 1;
        }
    }
    mean_treat = mean_treat/n_treat;
    mean_control = mean_control/n_control;
    double tau = mean_treat - mean_control;
    return tau;
}

// Function to check minsize requirement 
bool check_minsize (std::vector<int>& vec, int& minsize) {
    int sum = 0;
    for(int i = 0; i < vec.size(); ++i) {
        sum += vec[i];
    }

    if(sum >= minsize && (vec.size() - sum) >= minsize) {
        return true;
    } else {
        return false;
    }
}

// Function to compute components for cost function 
std::tuple<double,double,int> cost_elements(
    std::vector<double> &vec, 
    std::vector<int> &treat) {
    double mean_treat = 0.0;
    double mean_control = 0.0;
    int n_treat = 0;
    int n_control = 0;
    for(int i = 0; i < vec.size(); ++i) {
        if(treat[i] == 1) {
            mean_treat += vec[i];
            n_treat += 1;
        } else {
            mean_control += vec[i];
            n_control += 1;
        }
    }
    mean_treat = mean_treat/n_treat;
    mean_control = mean_control/n_control;

    double var_treat = 0.0;
    double var_control = 0.0;
    for(int i = 0; i < vec.size(); ++i) {
        if(treat[i] == 1) {
            var_treat += pow(vec[i] - mean_treat, 2.0);
        } else {
            var_control += pow(vec[i] - mean_control, 2.0);
        }
    } 
    var_treat = var_treat/n_treat;
    var_control = var_control/n_control;

    double tau = mean_treat - mean_control;
    double var = var_treat + var_control;
    int n = n_treat + n_control;

    return std::make_tuple(tau,var,n);
}

double cost(
    std::tuple<double,double,int>& left, 
    std::tuple<double,double,int>& right,
    double alpha) {
    double cost_1 = alpha * pow((std::get<0>(left) - std::get<0>(right)),2.0);
    double cost_2 = (1.0 - alpha) * ((std::get<1>(left) + std::get<1>(right))/(std::get<2>(left) + std::get<2>(right)));
    return (cost_1 - cost_2);
}

// Function to split the data based on a given feature and threshold
data_split splitData(
    std::vector<std::vector<double>> &X, 
    std::vector<double> &y, 
    std::vector<int> &treat, 
    int feature, 
    double threshold) {   
    std::vector<std::vector<double>> leftX, rightX;
    std::vector<double> lefty, righty;
    std::vector<int> lefttreat, righttreat;
    for (int i = 0; i < X.size(); ++i) {
        if (X[i][feature] < threshold) {
            leftX.push_back(X[i]);
            lefty.push_back(y[i]);
            lefttreat.push_back(treat[i]);
        }
        else {
            rightX.push_back(X[i]);
            righty.push_back(y[i]);
            righttreat.push_back(treat[i]);
        }
    }
    return std::make_pair(std::make_tuple(leftX, lefty,lefttreat), std::make_tuple(rightX, righty, righttreat));
}

// Function to find the best split for the data based on all features
std::pair<int, double> findBestSplit(
    std::vector<std::vector<double>> &X, 
    std::vector<double> &y, 
    std::vector<int> &treat,
    double alpha,
    int minsize) {
    int bestFeature = -1;
    double bestThreshold, bestScore = -DBL_MAX;
    for (int feature = 0; feature < X[0].size(); ++feature) {
        vector<double> featureValues;
        for (int i = 0; i < X.size(); ++i) {
            featureValues.push_back(X[i][feature]);
        }
        std::sort(featureValues.begin(), featureValues.end());
        featureValues.erase(std::unique(featureValues.begin(), featureValues.end()), featureValues.end());

        for (int i = 1; i < featureValues.size() - 1; ++i) {
            data_split split_i = splitData(X, y, treat, feature, featureValues[i]);

            if(check_minsize(std::get<2>(split_i.first), minsize) && check_minsize(std::get<2>(split_i.second), minsize)) {
                std::tuple<double,double,int> left_elements = cost_elements(std::get<1>(split_i.first),std::get<2>(split_i.first));
                std::tuple<double,double,int> right_elements = cost_elements(std::get<1>(split_i.second),std::get<2>(split_i.second));
                double score = cost(left_elements,right_elements,alpha);

                if (score > bestScore) {
                    bestScore = score;
                    bestFeature = feature;
                    bestThreshold = featureValues[i];
                }
            }
        }
    }
    return std::make_pair(bestFeature, bestThreshold);
}

// Function to build the regression tree
Node *buildTree(
    std::vector<std::vector<double>> &X, 
    std::vector<double> &y,
    std::vector<int> &treat,
    double alpha,
    int minsize){
    Node *node = new Node();
    auto [bestFeature, bestThreshold] = findBestSplit(X, y, treat, alpha, minsize);
    if (bestFeature == -1)
    {
        node->value = tau(y,treat);
        return node;
    }
    node->feature = bestFeature;
    node->threshold = bestThreshold;
    data_split split = splitData(X, y, treat, bestFeature, bestThreshold);
    node->left = buildTree(std::get<0>(split.first), std::get<1>(split.first), std::get<2>(split.first), alpha, minsize);
    node->right = buildTree(std::get<0>(split.second), std::get<1>(split.second), std::get<2>(split.second), alpha, minsize);
    return node;
}

// Function to predict the output for a given input data point
//double predict(Node *node, vector<double> &data)
//{
//    if (!node->left && !node->right)
//    {
//        return node->value;
//    }
//    if (data[node->feature] < node->threshold)
//    {
//        return predict(node->left, data);
//    }
//    else
//    {
//        return predict(node->right, data);
//    }
//}

// Function to print the tree in a readable format
void print_tree(Node *node, int indent = 0)
{
    if (!node)
    {
        return;
    }
    for (int i = 0; i < indent; i++)
    {
        cout << " ";
    }
    if (!node->left && !node->right)
    {
        cout << "Predict " << node->value << endl;
        return;
    }
    cout << "Feature " << node->feature << " < " << node->threshold << endl;
    print_tree(node->left, indent + 2);
    print_tree(node->right, indent + 2);
}

int main()
{
    std::vector<std::vector<double>> X = {{1, 2}, {3, 4}, {5, 6}, {7, 8}, {9, 10}, {11, 12}, {13, 14}, {15, 16}};
    std::vector<double> y = {1, 2, 3, 4, 5, 6, 7, 8};
    std::vector<int> treat = {0,0,1,1,0,0,1,1};
    Node *root = buildTree(X, y, treat, 0.5, 2);
    print_tree(root);
    //vector<double> test = {2, 3};
    //double pred = predict(root, test);
    //cout << "Prediction: " << pred << endl;
    return 0;
}
