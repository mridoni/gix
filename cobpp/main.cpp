#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <string>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm> 
#include <cctype>
#include <locale>


#if defined(_WIN32)
#define DIR_SEP "\\"
#define PATH_SEP ';'
#else
#define DIR_SEP "/"
#define PATH_SEP ':'
#endif

using namespace std;

char* outfile;
char* mapfile;

int rc = 0;
int verbose = 0;

vector<string> all_lines;
vector<string> copy_dirs;
vector<string> src_line_map;	// File index + line (orig) => line preprocessed, e.g. 177@1 => 278
int cur_output_line = 0;

vector<string> load_file(const char* f);
void get_copy_dirs();
void processNextFile(const char *);
bool is_copy_statement(string ln, string& copy_file);
bool resolve_copy_file(const string copy_name, string& copy_file);
int nlines = 0;

bool file_exists(char* filename)
{
	struct stat   buffer;
	return (stat(filename, &buffer) == 0);
}

// trim from start (in place)
static inline void ltrim(std::string& s)
{
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
		return !std::isspace(ch);
	}));
}

// trim from end (in place)
static inline void rtrim(std::string& s)
{
	s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
		return !std::isspace(ch);
	}).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string& s)
{
	ltrim(s);
	rtrim(s);
}

// trim from start (copying)
static inline std::string ltrim_copy(std::string s)
{
	ltrim(s);
	return s;
}

// trim from end (copying)
static inline std::string rtrim_copy(std::string s)
{
	rtrim(s);
	return s;
}

// trim from both ends (copying)
static inline std::string trim_copy(std::string s)
{
	trim(s);
	return s;
}

static inline bool startsWith(string s1, string s2)
{
	return s1.substr(0, s2.length()) == s2;
}

int main(int argc, char** argv)
{
	int ncopy = 0;
	char* main_file = NULL;

	if (argc != 4 && argc != 5) {
		fprintf(stderr, "Usage: cobpp [-v] <infile> <outfile> <outmapfile>\n");
		return 1;
	}

	if (argc == 5 && strcmp(argv[1], "-v") != 0) {
		fprintf(stderr, "Usage: cobpp [-v] <infile> <outfile> <outmapfile>\n");
		return 1;
	}

	if (argc == 4) {
		main_file = argv[1];
		outfile = argv[2];
		mapfile = argv[3];
	}

	if (argc == 5) {
		main_file = argv[2];
		outfile = argv[3];
		mapfile = argv[4];
		verbose = 1;
	}

	get_copy_dirs();

	processNextFile(main_file);

	if (!rc) {
		FILE* outf = fopen(outfile, "wb");
		for (string ln : all_lines) {
			fprintf(outf, "%s\n", ln.c_str());
		}
		fclose(outf);

		FILE* mapf = fopen(mapfile, "wb");
		vector<string>::iterator it;
		for (it = src_line_map.begin(); it != src_line_map.end(); ++it) {
			string key = *it;
			fprintf(mapf, "%s\n", key.c_str());
		}
		fclose(outf);
	}

	printf("lines: %d\n", nlines);

	return rc;
}

vector<string> load_file(const char* fname)
{
	char bfr[256];
	char bfr2[256];
	vector<string> res;
	

	FILE* f = fopen(fname, "rb");

	while (char* s = fgets(bfr, sizeof(bfr), f)) {
		int j = 0;
		for (int i = 0; i < strlen(bfr); i++) {
			if (bfr[i] != 0x1a)
				bfr2[j++] = bfr[i];
		}
		bfr2[j] = 0;

		for (int i = strlen(bfr2) - 1; i >= 0; i--) {
			if (bfr2[i] == 0x0d || bfr2[i] == 0x0a)
				bfr2[i] = 0x00;
		}
		res.push_back(string(bfr2));
	}

	fclose(f);

	return res;
}

void get_copy_dirs()
{
	const char *v = getenv("COBCPY");
	if (!v)
		return;

	string copy_dir = v;
	if (copy_dir.empty())
		return;

	std::stringstream ss(copy_dir);
	std::string token;
	while (std::getline(ss, token, PATH_SEP)) {
		copy_dirs.push_back(token);
	}
}


void processNextFile(const char *filename)
{
	string copy_name, copy_file;

	if (verbose)
		printf("Processing %s\n", filename);

	vector<string> file_lines = load_file(filename);
	nlines += file_lines.size();

	for (int i = 0; i < file_lines.size(); i++) {
		string ln = file_lines.at(i);
		if (is_copy_statement(ln, copy_name)) {
			if (resolve_copy_file(copy_name, copy_file)) {
				processNextFile(copy_file.c_str());
			}
			else {
				fprintf(stderr, "Error: cannot locate copy file [%s]\n", copy_name.c_str());
				exit(1);
			}
		}
		else {
			cur_output_line++;
			all_lines.push_back(ln);
			string key = to_string(cur_output_line) + "|" + std::to_string(i + 1) + "@" + filename;
			src_line_map.push_back(key);
		}

	}
}

bool is_copy_statement(const string line, string& copy_name)
{
	string ln = line;

	if (ln.length() < 7)
		return false;;

	if (ln[6] == '*')
		return false;

	ln = string(ln.substr(7));
	trim(ln);

	if (startsWith(ln, "COPY ")) {
		string cname = ln.substr(5);
		if (cname[cname.length() - 1] == '.')
			cname = cname.substr(0, cname.length() - 1);

		trim(cname);

		copy_name = cname;
		return true;
	}

	return false;
}

bool resolve_copy_file(const string copy_name, string& copy_file)
{
	if (verbose)
		printf("Trying %s\n", copy_name.c_str());

	if (file_exists((char*)copy_name.c_str())) {
		copy_file = copy_name;
		return true;
	}

	if (verbose)
		printf("Trying %s\n", (copy_name + ".CPY").c_str());

	if (file_exists((char*)(copy_name + ".CPY").c_str())) {
		copy_file = copy_name + ".CPY";
		return true;
	}

	if (verbose)
		printf("Trying %s\n", (copy_name + ".cpy").c_str());

	if (file_exists((char*)(copy_name + ".cpy").c_str())) {
		copy_file = copy_name + ".cpy";
		return true;
	}

	if (copy_dirs.empty())
		return false;

	for (string copy_dir : copy_dirs) {
		string cn = copy_dir + DIR_SEP + trim_copy(copy_name);

		if (verbose)
			printf("Trying %s\n", cn.c_str());

		if (file_exists((char*)cn.c_str())) {
			copy_file = cn;
			return true;
		}

		if (verbose)
			printf("Trying %s\n", (cn + ".CPY").c_str());

		if (file_exists((char*)(cn + ".CPY").c_str())) {
			copy_file = cn + ".CPY";
			return true;
		}

		if (verbose)
			printf("Trying %s\n", (cn + ".CPY").c_str());

		if (file_exists((char*)(cn + ".cpy").c_str())) {
			copy_file = cn + ".cpy";
			return true;
		}
	}
	return false;
}