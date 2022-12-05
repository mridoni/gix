#include "ErrorWarningFilter.h"

#include <QRegularExpression>

static QRegularExpression rxWarning = QRegularExpression(R"(^(.*)\(([0-9]+)\)\:\ warning\:\ (.*)$)");
static QRegularExpression rxError = QRegularExpression(R"(^(.*)\(([0-9]+)\)\:\ error\:\ (.*)$)");
static QRegularExpression rxParagraph = QRegularExpression(R"(^(.*)\:\ in\ paragraph\ \'([A-Za-z0-9\-]+)\'\:$)");
static QRegularExpression rxSection = QRegularExpression(R"(^(.*)\:\ in\ section\ \'([A-Za-z0-9\-]+)\'\:$)");


QList<ErrorWarningFilterEntry> ErrorWarningFilter::filter(QString payload, spdlog::level::level_enum level)
{
	QList<ErrorWarningFilterEntry> res;
	QStringList lines = payload.replace("\r", "").split("\n");
	QRegularExpressionMatch m;
	bool b;

	for (auto line : lines) {

		auto cl = strdup(line.toLocal8Bit().constData());
		m = rxWarning.match(line);
		if (m.hasMatch()) {
			ErrorWarningFilterEntry e;
			e.type = ErrorWarningFilterType::Warning;
			e.filename = m.captured(1);
			e.line = m.captured(2).toInt(&b);
			e.message = m.captured(3);
			if (b) {
				res.append(e);
			}
		}
		else {
			m = rxError.match(line);
			if (m.hasMatch()) {
				ErrorWarningFilterEntry e;
				e.type = ErrorWarningFilterType::Error;
				e.filename = m.captured(1);
				e.line = m.captured(2).toInt(&b);
				e.message = m.captured(3);
				if (b) {
					res.append(e);
				}
			}
			else {
				m = rxParagraph.match(line);
				if (m.hasMatch()) {
					ErrorWarningFilterEntry e;
					e.type = ErrorWarningFilterType::ParagrapHeader;
					e.filename = m.captured(1);
					e.line = 0;
					e.section_or_paragraph = m.captured(2);
					res.append(e);
				}
				else {
					m = rxSection.match(line);
					if (m.hasMatch()) {
						ErrorWarningFilterEntry e;
						e.type = ErrorWarningFilterType::SectionHeader;
						e.filename = m.captured(1);
						e.line = 0;
						e.section_or_paragraph = m.captured(2);
						res.append(e);
					}

				}
			}
		}
	}
	return res;
}

bool ErrorWarningFilter::isWarning(const QString& msg)
{
	return rxWarning.match(msg).hasMatch();
}

bool ErrorWarningFilter::isError(const QString& msg)
{
	return rxError.match(msg).hasMatch();
}
