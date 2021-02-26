#pragma once

#include <string>

#define SIGN_LENGTH 1
#define TERMINAL_LENGTH 1
#define DECIMAL_LENGTH 1

#if (defined(_WIN32) || defined(_WIN64)) && !defined(__MINGW32__)
#define MEM_FREE(x) {free(x); x=NULL;}
#endif

#define BUFFSIZE 256

char *trim_end(char *);
int strim(char * buf);
char *safe_strdup(char * s);
bool is_commit_or_rollback_statement(std::string query);
bool is_dml_statement(std::string query);
bool  is_select_statement(std::string query);
bool is_begin_transaction_statement(std::string query);
bool is_update_or_delete_statement(std::string query);
bool has_where_current_of(const std::string query, std::string& cursor_name, int* p);
inline void ltrim(std::string &s);

// trim from end (in place)
inline void rtrim(std::string &s);

// trim from both ends (in place)
inline void trim(std::string &s);

// trim from start (copying)
inline std::string ltrim_copy(std::string s);

// trim from end (copying)
inline std::string rtrim_copy(std::string s);

// trim from both ends (copying)
std::string trim_copy(std::string s);

inline bool starts_with(std::string s, std::string s1);

bool caseInsensitiveStringCompare(const std::string& str1, const std::string& str2);

std::string to_lower(const std::string s); 
std::string to_upper(const std::string s);

int find_nocase(const std::string needle, const std::string haystack);
