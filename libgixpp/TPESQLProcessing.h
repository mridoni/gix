#pragma once

#include <QString>
#include <QStringList>
#include <QMap>

#include "ITransformationStep.h"
#include "ESQLDefinitions.h"
#include "gix_esql_driver.hh"
#include "ESQLCall.h"

class gix_esql_driver;
//class ESQLCall;
enum class ESQL_Command;

class TPESQLProcessing : public ITransformationStep
{
public:
	TPESQLProcessing(GixPreProcessor *gpp);

	QString getModuleName();

	QMap<QString, QString> &getSrcLineMap() const;
	QMap<QString, QString> &getSrcLineMapReverse() const;

	QMap<uint64_t, uint64_t> &getBinarySrcLineMap() const;
	QMap<uint64_t, uint64_t> &getBinarySrcLineMapReverse() const;

	QMap<QString, int> &getFileMap() const;
	QMap<int, QString> getReverseFileMap();
	QMap<QString, cb_field_ptr> &getVariableDeclarationInfoMap() const;

	QMap<QString, srcLocation> getParagraphs();
	QMap<QString, QStringList> getFileDependencies();

	// Inherited via ITransformationStep
	virtual bool run(ITransformationStep *prev_step) override;
	virtual QString getOutput(ITransformationStep *me = nullptr) override;


private:

	// ESQL options
	bool opt_anonymous_params;
	bool opt_preprocess_copy_files;
	bool opt_emit_static_calls;
	bool opt_emit_debug_info;
	bool opt_emit_compat;
	bool opt_consolidated_map;
	bool opt_no_output;

	gix_esql_driver main_module_driver;

	QStringList output_lines;

	int output_line;

	QMap<QString, QString> in_to_out;
	QMap<QString, QString> out_to_in;

	QMap<uint64_t, uint64_t> b_in_to_out;
	QMap<uint64_t, uint64_t> b_out_to_in;

	int outputESQL();
	cb_exec_sql_stmt_ptr find_exec_sql_stmt(const QString filename, int i);
	QString comment_line(const QString &comment, const QString &line);

	void put_output_line(const QString &line);
	bool handle_esql_stmt(const ESQL_Command cmd, const cb_exec_sql_stmt_ptr stmt, bool is_in_ws);

	bool find_working_storage(int *working_begin_line, int *working_end_line);

	bool processNextFile();

	QString get_call_id(const QString s);

	void put_start_exec_sql(bool with_period);
	void put_end_exec_sql(bool with_period);
	void put_query_defs();
	void put_procedure_division();
	void put_working_storage();
	bool put_cursor_declarations();
	void put_call(const ESQLCall &call, bool terminate_with_period);

	bool is_var_len_group(cb_field_ptr f);
	bool get_actual_field_data(cb_field_ptr f, int *type, int *size, int *scale);
	void process_sql_query_list();

	bool write_map_file(const QString &preprocd_file);
	bool build_map_data();

	void map_collect_files(QMap<QString, int> &filemap);

	void splitLineEntry(const QString &k, QString &s, int *i);

	bool generate_consolidated_map();

	void add_dependency(const QString &parent, const QString &dep_path);

	QStack<QString> input_file_stack;
	int working_begin_line;
	int working_end_line;
	QString code_tag;

	QStringList ws_query_list;
	QList<cb_exec_sql_stmt_ptr> startup_items;

	QMap<QString, int> filemap;

	QMap<QString, QStringList> file_dependencies;

	int current_input_line;
};

