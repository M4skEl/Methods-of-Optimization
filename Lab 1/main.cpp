#include <iostream>
#include <vector>
#include <string>
#include <map>

using std::cin;
using std::cout;
using std::endl;
using std::vector;
using std::string;

void InputCondition(vector<vector<double>> &mat,
                    vector<double> &coef,
                    vector<double> &free,
                    bool &flag,
                    double &startCond) {
  cout << "Input we need to find max or min of function?" << endl;
  cin >> flag;
  cout << "Input NUMBER of coefficient of your Function" << endl;

  size_t coefNumber;
  cin >> coefNumber;
  cout << "Input coefficients of your Function" << endl;

  double buffer = 0;
  for (auto i = 0; i < coefNumber; i++) {
    cin >> buffer;
    if (flag) coef.push_back(-buffer);
    else coef.push_back(buffer);
  }
  cout << "Input start condition F(0) of your Function" << endl;
  cin >> startCond;
  buffer = 0;
  cout << "Input NUMBER of freedom coefficient" << endl;

  size_t freeNumber;
  cin >> freeNumber;
  cout << "Input coefficients of your Function" << endl;
  for (auto i = 0; i < freeNumber; i++) {
    cin >> buffer;
    free.push_back(buffer);
  }
  buffer = 0;
  cout << "Input NUMBER of equality" << endl;

  size_t str;
  cin >> str;
  cout << "Input Number of columns" << endl;

  size_t columns;
  cin >> columns;
  vector<double> bufVec;
  for (auto i = 0; i < str; i++) {
    for (auto j = 0; j < columns; j++) {
      cout << "Input coefficients of X in matrix" << endl;
      cin >> buffer;
      bufVec.push_back(buffer);
    }
    mat.push_back(bufVec);
    bufVec.clear();
  }
}

void Output(vector<vector<double>> &mat, vector<double> &coef, vector<double> &free, double &find) {
  int a = 8;
  cout.setf(std::ios::left);
  for (auto i = 0; i < free.size(); i++) {
    cout.width(a);
    cout << free[i] << "|";
    for (auto j = 0; j < mat[i].size(); j++) {
      cout.width(a);
      cout << mat[i][j] << "|";
    }
    cout << endl;
  }
  cout.width(a);
  cout << find << "|";
  for (auto j = 0; j < coef.size(); j++) {
    cout.width(a);
    cout << coef[j] << "|";
  }
  cout << endl;
}

size_t DoSimplexRelations(std::pair<size_t, size_t> &res, vector<double> &freed, vector<vector<double>> &mat) {
  std::map<double, size_t> relation;
  for (size_t j = 0; j < freed.size(); j++) {
    if (freed[j] / mat[j][res.second] > 0)
      relation[freed[j] / mat[j][res.second]] = j;
  }
  return relation.begin()->second;
}

std::pair<size_t, size_t>
FindResolutionElement(vector<vector<double>> &mat, vector<double> &coef, vector<double> &free) {
  std::pair<size_t, size_t> resolut = {0, 0};
  double buffer = 0;
  for (size_t i = 0; i < coef.size(); i++) {
    if (buffer > coef[i]) {
      buffer = coef[i];
      resolut.second = i;
    }
  }
  resolut.first = DoSimplexRelations(resolut, free, mat);
  return resolut;
}

void RefreshSimplexTable(vector<vector<double>> &mat, vector<double> &coef, vector<double> &free,
                         std::pair<size_t, size_t> &res, double &funcValue) {

  //Перерасчет элементов не на разрешающем столбце и строке
  for (size_t i = 0; i < mat.size(); i++) {
    if (i != res.first) {
      for (size_t j = 0; j < mat[0].size(); j++) {
        if (j != res.second) {
          mat[i][j] = (mat[i][j] * mat[res.first][res.second] - mat[res.first][j] * mat[i][res.second]) /
                      mat[res.first][res.second];
        }
      }
    }
  }

  //Расчет нового значения функции
  funcValue =
          (funcValue * mat[res.first][res.second] - free[res.first] * coef[res.second]) / mat[res.first][res.second];

  //Расчет значения коэффициентов свободных членов
  {
    for (size_t i = 0; i < free.size(); i++) {
      if (i != res.first)
        free[i] = (free[i] * mat[res.first][res.second] - free[res.first] * mat[i][res.second]) /
                  mat[res.first][res.second];
    }
    free[res.first] /= mat[res.first][res.second];
  }
//Расчет значения коэффициентов функции
  for (size_t i = 0; i < coef.size(); i++) {
    if (i != res.second)
      coef[i] = (coef[i] * mat[res.first][res.second] - coef[res.second] * mat[res.first][i]) /
                mat[res.first][res.second];
  }
  coef[res.second] /= -mat[res.first][res.second];

  //разрешающаяся строка делится на разрешающий элемент
  for (size_t i = 0; i < coef.size(); i++) {
    if (i != res.second) mat[res.first][i] /= mat[res.first][res.second];
  }
  //разрешающийся столбец делится на элемент противоположный разрешающему
  for (size_t i = 0; i < mat.size(); i++) {
    if (i != res.first) mat[i][res.second] /= -mat[res.first][res.second];
  }
  //разрешающий элемент заменяется на обратный
  mat[res.first][res.second] = 1 / mat[res.first][res.second];
}

bool ContinueOptimized(vector<double> &coef) {
  for (auto c:coef)
    if (c < 0) return true;
  return false;
}

vector<double> DoAnswer(vector<vector<int>> &variables, vector<double> &free) {
  vector<double> ans;
  for (size_t i = 0; i < variables.size(); i++) {
    ans.push_back(variables[i][0] * free[variables[i][1]]);
  }
  return ans;
}

std::pair<double, vector<double>> SimplexMethod(vector<double> &forcheck) {
  vector<double> coeffs;
  vector<double> freedom;
  vector<vector<double>> matrix;
  bool max;
  double finder = 0;
  //InputCondition(matrix, coeffs, freedom,finder);
  matrix = {{1,  -2,},
            {-2, 1},
            {1,  1}};
  coeffs = {-1, 1};
  freedom = {2, -2, 5};
/*  matrix = {{4, 1,   1},
            {1, 2,   0},
            {0, 0.5, 3}};
  coeffs = {-7, -4, -4};
  freedom = {3, 3, 5};*/
  max = false;
  forcheck=coeffs;
  if (max) for (size_t t = 0; t < coeffs.size(); t++) forcheck[t] = -coeffs[t];
  /*  matrix = {{4, 2, 1},
           {3, 1, 3},
           {1, 2, 5}};
 coeffs = {-10, -14, -12};
 freedom = {180, 210, 244};*/
  /*matrix = {{2,  1, 1},
            {1, 2, 0},
            {0,  0.5, 1}};
  coeffs = {-2, 8, 3};
  freedom = {4, 6, 2};*/


  vector<vector<int>> solve;
  solve.resize(coeffs.size());
  for (size_t i = 0; i < coeffs.size(); i++) {
    for (size_t j = 0; j < 3; j++)
      solve[i].push_back(-1);
  }
  std::pair<size_t, size_t> resolut;
  Output(matrix, coeffs, freedom, finder);
  cout << endl;

  bool proceed = true;
  int k = 0;
  while (proceed) {
    resolut = FindResolutionElement(matrix, coeffs, freedom);
    cout << resolut.first << ' ' << resolut.second << endl;
    RefreshSimplexTable(matrix, coeffs, freedom, resolut, finder);
    Output(matrix, coeffs, freedom, finder);
    cout << endl;

    int count = 0;
    for (size_t i = 0; i < solve.size(); i++) {
      if (solve[i][1] == resolut.first || solve[i][2] == resolut.second) count++;
    }
    if (count == 0) {
      solve[k][0] = 1;
      solve[k][1] = resolut.first;
      solve[k][2] = resolut.second;
      k++;
    }
    proceed = ContinueOptimized(coeffs);
  }
  for (size_t i = 0; i < solve.size(); i++) {

    if (solve[i][1] < 0) solve[i][0] = 0;
    if (solve[i][2] < 0) solve[i][0] = 0;

  }
  vector<double> answers = DoAnswer(solve, freedom);
  if (!max) finder = -finder;
  std::pair<double, vector<double>> result = {finder, answers};
  return result;
}

void OutputAnswer(std::pair<double, vector<double>> &ans) {
  cout << "F(";
  for (size_t i = 0; i < ans.second.size(); i++) cout << " " << ans.second[i] << ";";
  cout << ") = " << ans.first << endl;
}

double Cheking(std::pair<double, vector<double>> &ans, vector<double> &check) {
  double repl = 0;
  for (size_t i = 0; i < ans.second.size(); i++) repl += ans.second[i] * check[i];
  return repl;
}

int main() {
  vector<double> check;
  std::pair<double, vector<double>> answer = SimplexMethod(check);
  double forRight = Cheking(answer, check);
  OutputAnswer(answer);
  std::cout << forRight << endl;
  return 0;
}
