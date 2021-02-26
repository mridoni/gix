#include "CopyResolver.h"

#include <QFile>
#include <QDir>

CopyResolver::CopyResolver(const QStringList &_copy_dirs)
{
	copy_dirs = _copy_dirs;
}

CopyResolver::CopyResolver()
{

}

void CopyResolver::resetCache()
{
	resolve_cache.clear();
}

void CopyResolver::setCopyDirs(const QStringList &_copy_dirs)
{
	QString cd = _copy_dirs.join(QDir::listSeparator());
	if (cd == hash)
		return;

	resolve_cache.clear();
	copy_dirs = _copy_dirs;
	hash = cd;
}

void CopyResolver::setExtensions(const QStringList &_copy_exts)
{
	copy_exts = _copy_exts;
}

void CopyResolver::setBaseDir(const QString _base_dir)
{
	base_dir = _base_dir;
}

QStringList &CopyResolver::getCopyDirs() const
{
	return const_cast<QStringList&>(copy_dirs);
}

bool CopyResolver::resolveCopyFile(const QString copy_name, QString &copy_file)
{
	QFile the_file;

	if (copy_name.isEmpty()) {
		fprintf(stderr, "Invalid copy name\n");
		return false;
	}

	if (resolve_cache.contains(copy_name)) {
		copy_file = resolve_cache[copy_name];
		return true;
	}

	for (QString ext : copy_exts) {

		if (ext == ".")
			ext = "";

		the_file.setFileName(base_dir + QDir::separator() + copy_name + ext);
		if (the_file.exists()) {
			copy_file = QFileInfo(the_file).absoluteFilePath();
			resolve_cache[copy_name] = copy_file;
			return true;
		}
	}

	if (copy_dirs.empty())
		return false;

	for (QString copy_dir : copy_dirs) {
		QString cn = copy_dir + QDir::separator() + copy_name.trimmed();

		for (QString ext : copy_exts) {

			if (ext == ".")
				ext = "";

			the_file.setFileName(cn + ext);
			if (the_file.exists()) {
				copy_file = QFileInfo(the_file).absoluteFilePath();
				resolve_cache[copy_name] = copy_file;
				return true;
			}
		}

	}

	return false;
}