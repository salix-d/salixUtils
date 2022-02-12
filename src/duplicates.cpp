#include <Rcpp.h>
using namespace Rcpp;

// ? slower than base R at ~100+ but then uses much less memory
// [[Rcpp::export]]
std::vector<int> C_duplicates_int(std::vector<int> x){
  std::set<int> seen;
  std::set<int> seenOut;
  std::vector<int> out;
  std::copy_if(x.begin(), x.end(), std::back_inserter(out),
               [=, &seen, &seenOut](int i){
                 if(!seen.insert(i).second) return seenOut.insert(i).second;
                 else return false;
               });
  return out;
}
// [[Rcpp::export]]
std::vector<double> C_duplicates_dbl(std::vector<double> x){
  std::set<double> seen;
  std::set<double> seenOut;
  std::vector<double> out;
  std::copy_if(x.begin(), x.end(), std::back_inserter(out),
               [=, &seen, &seenOut](double i){
                 if(!seen.insert(i).second) return seenOut.insert(i).second;
                 else return false;
               });
  return out;
}
// [[Rcpp::export]]
std::vector<bool> C_duplicates_log(std::vector<bool> x){
  std::set<bool> seen;
  std::set<bool> seenOut;
  std::vector<bool> out;
  std::copy_if(x.begin(), x.end(), std::back_inserter(out),
               [=, &seen, &seenOut](bool i){
                 if(!seen.insert(i).second) return seenOut.insert(i).second;
                 else return false;
               });
  return out;
}
// [[Rcpp::export]]
std::vector<std::string> C_duplicates_char(std::vector<std::string> x){
  std::set<std::string> seen;
  std::set<std::string> seenOut;
  std::vector<std::string> out;
  std::copy_if(x.begin(), x.end(), std::back_inserter(out),
               [=, &seen, &seenOut](std::string i){
                 if(!seen.insert(i).second) return seenOut.insert(i).second;
                 else return false;
               });
  return out;
}
