#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <cstdint>
#include <algorithm>
#include <climits>
#include <cmath>
#include <stdexcept>

using namespace std;

// ---------- простой парсер JSON ----------
string read_file(const string& filename) {
    ifstream f(filename);
    if (!f) {
        throw runtime_error("Не удалось открыть " + filename);
    }
    stringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

void skip_ws(const string& s, int& pos) {
    while (pos < s.size() && (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '\n' || s[pos] == '\r'))
        ++pos;
}

string parse_string(const string& s, int& pos) {
    skip_ws(s, pos);
    if (s[pos] != '"') throw runtime_error("Ожидался символ '\"'");
    ++pos;
    string res;
    while (pos < s.size() && s[pos] != '"') {
        if (s[pos] == '\\') { ++pos; }
        res += s[pos++];
    }
    if (pos < s.size()) ++pos;
    return res;
}

int parse_int(const string& s, int& pos) {
    skip_ws(s, pos);
    int sign = 1;
    if (s[pos] == '-') { sign = -1; ++pos; }
    if (!isdigit(s[pos])) throw runtime_error("Ожидалась цифра");
    int val = 0;
    while (pos < s.size() && isdigit(s[pos])) {
        val = val * 10 + (s[pos++] - '0');
    }
    return val * sign;
}

void parse_wallet(const string& s, int& pos, vector<pair<int,int>>& wallet) {
    skip_ws(s, pos);
    if (s[pos] != '[') throw runtime_error("Ожидался символ '['");
    ++pos;
    if (s[pos] == ']') { ++pos; return; }
    while (true) {
        skip_ws(s, pos);
        if (s[pos] == '[') {
            ++pos;
            int a = parse_int(s, pos);
            skip_ws(s, pos);
            if (s[pos] == ',') ++pos;
            int b = parse_int(s, pos);
            skip_ws(s, pos);
            if (s[pos] == ']') ++pos;
            wallet.emplace_back(a, b);
        }
        skip_ws(s, pos);
        if (s[pos] == ']') { ++pos; break; }
        if (s[pos] == ',') { ++pos; continue; }
        throw runtime_error("Неожиданный символ в wallet");
    }
}

struct Input {
    vector<pair<int,int>> wallet;
    int amount;
    string strategy;
};

Input parse_input(const string& filename) {
    string data = read_file(filename);
    Input inp;
    int pos = 0;
    skip_ws(data, pos);
    
    bool in_array = false;
    if (pos < data.size() && data[pos] == '[') {
        in_array = true;
        ++pos;
        skip_ws(data, pos);
    }
    
    if (pos >= data.size() || data[pos] != '{')
        throw runtime_error("Ожидался объект JSON");
    ++pos;
    
    while (true) {
        skip_ws(data, pos);
        if (data[pos] == '}') break;
        string key = parse_string(data, pos);
        skip_ws(data, pos);
        if (data[pos] != ':') throw runtime_error("Ожидался символ ':'");
        ++pos;
        if (key == "wallet") {
            parse_wallet(data, pos, inp.wallet);
        } else if (key == "amount") {
            inp.amount = parse_int(data, pos);
        } else if (key == "strategy") {
            inp.strategy = parse_string(data, pos);
        } else {
            throw runtime_error("Неизвестный ключ: " + key);
        }
        skip_ws(data, pos);
        if (data[pos] == ',') { ++pos; continue; }
    }
    ++pos;
    
    if (in_array) {
        skip_ws(data, pos);
        if (pos < data.size() && data[pos] == ',') ++pos;
        skip_ws(data, pos);
        if (pos >= data.size() || data[pos] != ']')
            throw runtime_error("Ожидался символ ']' в конце массива");
        ++pos;
    }
    return inp;
}

void write_output(const string& filename, const vector<pair<int,int>>& dispense) {
    ofstream f(filename);
    f << "{\"dispense\":[";
    for (size_t i = 0; i < dispense.size(); ++i) {
        if (i > 0) f << ",";
        f << "[" << dispense[i].first << "," << dispense[i].second << "]";
    }
    f << "]}\n";
}

// ---------- Битовый набор для быстрой проверки достижимости (рюкзак)----------
struct BitSet {
    vector<uint64_t> bits;
    int n;
    BitSet(int size) : n(size), bits((size + 63) / 64, 0) {}
    void set(int pos) {
        bits[pos >> 6] |= (1ULL << (pos & 63));
    }
    bool test(int pos) const {
        return (bits[pos >> 6] >> (pos & 63)) & 1;
    }
    void shift_or(int shift) {
        if (shift <= 0 || shift >= n) return;
        int word_shift = shift / 64;
        int bit_shift = shift % 64;
        int words = bits.size();
        for (int i = words - 1; i >= 0; --i) {
            uint64_t low_part = 0;
            if (i >= word_shift) {
                low_part = bits[i - word_shift] << bit_shift;
                if (bit_shift && i - word_shift - 1 >= 0) {
                    low_part |= bits[i - word_shift - 1] >> (64 - bit_shift);
                }
            }
            bits[i] |= low_part;
        }
    }
};

// Проверка, можно ли набрать сумму amount заданными купюрами с ограничениями max_counts
bool can_make(int amount, const vector<int>& max_counts, const vector<int>& denoms) {
    BitSet reachable(amount + 1);
    reachable.set(0);
    for (size_t i = 0; i < denoms.size(); ++i) {
        int d = denoms[i];
        int c = max_counts[i];
        if (c == 0 || d > amount) continue;
        int k = 1;
        while (c > 0) {
            int take = min(k, c);
            int shift = take * d;
            if (shift <= amount) reachable.shift_or(shift);
            c -= take;
            k <<= 1;
        }
    }
    return reachable.test(amount);
}

// ---------- Стратегии MAX и MIN (жадный выбор с проверкой остатка) ----------
vector<pair<int,int>> solve_max_min_greedy(vector<pair<int,int>> wallet, int amount, const string& strat) {
    // Сортируем кошелёк в нужном порядке
    if (strat == "MAX") {
        sort(wallet.begin(), wallet.end(), [](auto& a, auto& b) { return a.first > b.first; });
    } else { // MIN
        sort(wallet.begin(), wallet.end(), [](auto& a, auto& b) { return a.first < b.first; });
    }
    
    int N = wallet.size();
    vector<int> counts(N, 0);
    int rem = amount;
    
    for (int i = 0; i < N; ++i) {
        int d = wallet[i].first;
        int max_avail = wallet[i].second;
        int max_take = min(max_avail, rem / d);
        
        // Пробуем взять максимально возможное количество, проверяя, что остаток можно добрать
        bool found = false;
        for (int take = max_take; take >= 0; --take) {
            int new_rem = rem - take * d;
            // Если это последний номинал или остаток 0 – достаточно
            if (new_rem == 0) {
                counts[i] = take;
                rem = new_rem;
                found = true;
                break;
            }
            if (i == N - 1) continue; // следующих номиналов нет
            
            // Формируем список оставшихся купюр
            vector<int> sub_denoms, sub_counts;
            for (int j = i + 1; j < N; ++j) {
                sub_denoms.push_back(wallet[j].first);
                sub_counts.push_back(wallet[j].second);
            }
            if (can_make(new_rem, sub_counts, sub_denoms)) {
                counts[i] = take;
                rem = new_rem;
                found = true;
                break;
            }
        }
        if (!found) return {}; // не удалось подобрать – решения нет
    }
    
    if (rem != 0) return {};
    
    vector<pair<int,int>> res;
    for (int i = 0; i < N; ++i) {
        if (counts[i] > 0)
            res.emplace_back(wallet[i].first, counts[i]);
    }
    return res;
}

// ---------- Стратегия UNIFORM ----------
vector<int> get_solution(int amount, const vector<int>& max_counts, const vector<int>& denoms) {
    int N = denoms.size();
    vector<int> dp(amount + 1, -1);
    vector<int> item(amount + 1, -1);
    dp[0] = 0;
    int chunk_idx = 0;
    vector<int> chunk_coin, chunk_take;
    for (int i = 0; i < N; ++i) {
        int d = denoms[i];
        int c = max_counts[i];
        int k =1;
        while (c > 0) {
            int take = min(k, c);
            int val = take * d;
            if (val <= amount) {
                chunk_coin.push_back(i);
                chunk_take.push_back(take);
                for (int s = amount; s >= val; --s) {
                    if (dp[s - val] != -1 && dp[s] == -1) {
                        dp[s] = s - val;
                        item[s] = chunk_idx;
                    }
                }
                ++chunk_idx;
            }
            c -= take;
            k <<= 1;
        }
    }
    if (dp[amount] == -1) return {};
    vector<int> counts(N, 0);
    int cur = amount;
    while (cur > 0) {
        int cidx = item[cur];
        int i = chunk_coin[cidx];
        int t = chunk_take[cidx];
        counts[i] += t;
        cur = dp[cur];
    }
    return counts;
}

vector<pair<int,int>> solve_uniform(const vector<pair<int,int>>& wallet, int amount) {
    int N = wallet.size();
    vector<int> denoms(N), orig_counts(N);
    int max_c = 0, min_c = INT_MAX;
    long long total_sum = 0;
    for (int i = 0; i < N; ++i) {
        denoms[i] = wallet[i].first;
        orig_counts[i] = wallet[i].second;
        max_c = max(max_c, orig_counts[i]);
        min_c = min(min_c, orig_counts[i]);
        total_sum += (long long)denoms[i] * orig_counts[i];
    }
    if (amount > total_sum) return {};

    const int INF = 1e9;
    int best_diff = INF;
    int best_M = -1, best_m = -1, best_D = -1;

    // Сценарий 1: минимум = 0, максимум <= M
    {
        int low = 0, high = max_c;
        while (low < high) {
            int mid = (low + high) / 2;
            vector<int> limits(N);
            for (int i = 0; i < N; ++i) limits[i] = min(orig_counts[i], mid);
            if (can_make(amount, limits, denoms))
                high = mid;
            else
                low = mid + 1;
        }
        vector<int> limits(N);
        for (int i = 0; i < N; ++i) limits[i] = min(orig_counts[i], low);
        if (can_make(amount, limits, denoms)) {
            best_diff = low;
            best_M = low;
        }
    }

    // Сценарий 2: используются все номиналы, минимум = m >= 1, максимум <= m + D
    int max_m = min(1000, min_c);
    for (int m = 1; m <= max_m; ++m) {
        if (amount < (long long)m * N) break;
        int D_low = 0, D_high = max_c - m;
        long long max_possible = 0;
        for (int i = 0; i < N; ++i) max_possible += min(orig_counts[i], m + D_high);
        if (max_possible < amount) continue;
        while (D_low < D_high) {
            int mid = (D_low + D_high) / 2;
            long long s = 0;
            for (int i = 0; i < N; ++i) s += min(orig_counts[i], m + mid);
            if (s >= amount)
                D_high = mid;
            else
                D_low = mid + 1;
        }
        for (int D = D_low; D <= max_c - m; ++D) {
            vector<int> limits(N);
            long long cap = 0;
            for (int i = 0; i < N; ++i) {
                int up = min(orig_counts[i], m + D);
                limits[i] = up - m;
                cap += limits[i];
            }
            if (cap < amount - (long long)m * N) continue;
            if (can_make(amount - m * N, limits, denoms)) {
                if (D < best_diff) {
                    best_diff = D;
                    best_m = m;
                    best_D = D;
                }
                break;
            }
        }
    }

    if (best_diff == INF) return {};

    vector<int> result_counts;
    if (best_M != -1) {
        vector<int> limits(N);
        for (int i = 0; i < N; ++i) limits[i] = min(orig_counts[i], best_M);
        result_counts = get_solution(amount, limits, denoms);
    } else {
        int m = best_m, D = best_D;
        vector<int> limits(N);
        for (int i = 0; i < N; ++i) limits[i] = min(orig_counts[i], m + D) - m;
        vector<int> y = get_solution(amount - m * N, limits, denoms);
        result_counts.resize(N);
        for (int i= 0; i < N; ++i) result_counts[i] = y[i] + m;
    }

    vector<pair<int,int>> dispense;
    for (int i = 0; i < N; ++i)
        if (result_counts[i] > 0)
            dispense.emplace_back(denoms[i], result_counts[i]);
    return dispense;
}

// ---------- главная функция ----------
int main() {
    vector<pair<int,int>> dispense;
    try {
        Input inp = parse_input("input.json");

        if (inp.strategy == "MAX" || inp.strategy == "MIN") {
            dispense = solve_max_min_greedy(inp.wallet, inp.amount, inp.strategy);
        } else if (inp.strategy == "UNIFORM") {
            dispense = solve_uniform(inp.wallet, inp.amount);
        }
    } catch (const exception& e) {
        cerr << "Ошибка: " << e.what() << endl;
        dispense.clear();
    }
    write_output("output.json", dispense);
    return 0;
}
