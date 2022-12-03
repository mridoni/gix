#include "OutputWindowLogger.h"
#include "IdeLogManager.h"
#include "ide_sink.h"

static std::vector<std::string> names = { "ide", "build", "debug" };


OutputWindowLogger::OutputWindowLogger(OutputWindowPaneType index, QWidget* w)
{
	pane = new QTextEdit(w);
	pane->setReadOnly(true);
	QFont f = pane->font();
	//f.setFamily("Courier New");
	f.setFamily("Consolas");
	f.setPointSize(UiUtils::computeFontSize(w, 10));
	pane->setFont(f);
	pane->setVisible(false);

	auto sink = std::make_shared<ide_sink_mt>(this);
	//sink->setOutputWindowLogger(this);

	std::vector<spdlog::sink_ptr> sinks = { sink };
	std::string logger_name = "gix-ide-" + (names.at((int)index));
	logger = std::make_shared<spdlog::logger>(logger_name, begin(sinks), end(sinks));

	sink->set_level(spdlog::level::trace);
	spdlog::set_level(spdlog::level::trace);	// max log level, will be limited by the sink-specific levels

	logger->flush_on(spdlog::level::trace);
}

QTextEdit* OutputWindowLogger::getWindowPane() const
{
	return pane;
}

std::shared_ptr<spdlog::logger> OutputWindowLogger::getLogger() const
{
	return logger;
}