#pragma once


#include <memory>
#include <spdlog/spdlog.h>

#include <QTextEdit>

#include "UiUtils.h"

enum class OutputWindowPaneType {
	Ide = 0,
	Build = 1,
	Debug = 2
};

class OutputWindowLogger {

public:

	OutputWindowLogger(OutputWindowPaneType index, QWidget* w = nullptr);

	QTextEdit* getWindowPane() const;
	std::shared_ptr<spdlog::logger> getLogger() const;

private:
	QTextEdit* pane;
	std::shared_ptr<spdlog::logger> logger;
};
