#pragma once

#include <QString>

#define SET_ERR(I,S) owner->err_code = I; owner->err_messages << QCoreApplication::translate("gix", S)

class ITransformationStep;
class GixPreProcessor;

class ITransformationStep
{
public:

	virtual bool run(ITransformationStep* prev_step) = 0;
	virtual QString getInput();
	virtual QString getOutput(ITransformationStep* me = nullptr);

	virtual void setInput(QString in_file);
	virtual void setOutput(QString out_file);


protected:

	ITransformationStep(GixPreProcessor* gpp);

	GixPreProcessor* owner;
	QString input_file;
	QString output_file;

};

