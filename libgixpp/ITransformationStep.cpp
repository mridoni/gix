#include "ITransformationStep.h"
#include "GixPreProcessor.h"

void ITransformationStep::setInput(QString in_file)
{
	input_file = in_file;
}

QString ITransformationStep::getInput()
{
	return input_file;
}

QString ITransformationStep::getOutput(ITransformationStep *me)
{
	return output_file;
}

void ITransformationStep::setOutput(QString out_file)
{
	output_file = out_file;
}

ITransformationStep::ITransformationStep(GixPreProcessor* gpp)
{
	this->owner = gpp;
	this->owner->err_code = 0;
	this->owner->err_messages.clear();
}