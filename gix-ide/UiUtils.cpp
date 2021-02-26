#include "UiUtils.h"

#include <QMessageBox>
#include <QApplication>
#include <QScreen>
#include <QSettings>
#include <QTimer>

#include <functional>

void UiUtils::dispatchToMainThread(std::function<void()> callback)
{
    // any thread
    QTimer* timer = new QTimer();
    timer->moveToThread(qApp->thread());
    timer->setSingleShot(true);
    QObject::connect(timer, &QTimer::timeout, [=]()
    {
        // main thread
        callback();
        timer->deleteLater();
    });
    QMetaObject::invokeMethod(timer, "start", Qt::QueuedConnection, Q_ARG(int, 0));
}

bool UiUtils::isOceCompatModeEnabled()
{
    QSettings settings;
    return settings.value("oce_compat_mode", false).toBool();
}

UiUtils::UiUtils()
{
}


UiUtils::~UiUtils()
{
}

void UiUtils::ErrorDialog(QString msg)
{
    dispatchToMainThread( [&, msg]{
        QMessageBox msgBox;
        msgBox.setText(msg);
        msgBox.setIcon(QMessageBox::Critical);
        msgBox.exec();
    });
}

void UiUtils::InfoDialog(QString msg)
{
    dispatchToMainThread( [&, msg]{
        QMessageBox msgBox;
        msgBox.setText(msg);
        msgBox.setIcon(QMessageBox::Information);
        msgBox.exec();
    });
}

bool UiUtils::YesNoDialog(QString msg)
{
   // dispatchToMainThread( [&, msg]{
        QMessageBox msgBox;
        msgBox.setText(msg);
        msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No);
        msgBox.setIcon(QMessageBox::Question);
        return msgBox.exec() == QMessageBox::Yes;
   // });
}

inline int UiUtils::OnPlatform(int _win, int _linux, int _osx)
{
#if defined(Q_OS_WIN)
	return _win;
#elif defined(Q_OS_LINUX)
	return _linux;
#elif defined(Q_OS_MAC)
	return _osx;
#else
#error "Unknown platform"
#endif
}

inline QString UiUtils::OnPlatform(QString _win, QString _linux, QString _osx)
{
#if defined(Q_OS_WIN)
	return _win;
#elif defined(Q_OS_LINUX)
	return _linux;
#elif defined(Q_OS_MAC)
	return _osx;
#else
#error "Unknown platform"
#endif
}

int UiUtils::computeFontSize(QWidget *w, int v)
{
	qreal screenDPI = QApplication::primaryScreen()->physicalDotsPerInch();

#if defined(Q_OS_WIN)
	qreal RENDER_DPI = w->logicalDpiY();
#elif defined(Q_OS_LINUX)
    qreal RENDER_DPI = 96;
#elif defined(Q_OS_MAC)
    qreal RENDER_DPI = 56;
#else
#error "Unknown platform"
#endif

	int pixelSize = (int)((qreal)v * screenDPI / RENDER_DPI);
	return pixelSize;
}

double ptToPx(double pt) {
	double dpi = QGuiApplication::primaryScreen()->physicalDotsPerInch();
	return pt / 72 * dpi;
}

double pxToPt(double px) {
	double dpi = QGuiApplication::primaryScreen()->physicalDotsPerInch();
	return px * 72 / dpi;
}

void UiUtils::setTreeViewFont(QTreeView *tv)
{
	QSettings settings;
	QString font_name = settings.value("treeview_font_name", DEFAULT_TREEVIEW_FONT_NAME).toString();
	int font_size = settings.value("treeview_font_size", DEFAULT_TREEVIEW_FONT_SIZE).toInt();
	
	QFont ff = tv->font();
	ff.setFamily(font_name);
	ff.setPointSize(font_size);
	tv->setFont(ff);

	auto px = ptToPx(font_size);

	tv->setIconSize(QSize(px + 4 , px + 4));
}
