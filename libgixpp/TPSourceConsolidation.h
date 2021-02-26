#pragma once

#include "ITransformationStep.h"

#include <QString>
#include <QStringList>
#include <QMap>
#include <QStack>

class TPSourceConsolidation : public ITransformationStep
{
public:
	TPSourceConsolidation(GixPreProcessor* gpp);

	// Inherited via ITransformationStep
	virtual bool run(ITransformationStep* prev_step) override;

	virtual QString getOutput(ITransformationStep* me = nullptr) override;

	QMap<QString, QString> &getSrcLineMap() const;
	QMap<int, QString> &getFileMap() const;

private:
	QStringList all_lines;

	QMap<int, QString> filemap;
	QMap<QString, QString> in_to_out;

	QStack<QString> input_file_stack;

	int current_input_line;
	int cur_output_line;
	int nlines;
	int output_line;

	bool map_only;

	bool processNextFile();
	void put_output_line(const QString &line);
	bool is_copy_statement(const QString line, QString &copy_name);
};

