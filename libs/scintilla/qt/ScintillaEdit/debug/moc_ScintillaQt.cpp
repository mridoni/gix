/****************************************************************************
** Meta object code from reading C++ file 'ScintillaQt.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.14.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include <memory>
#include "../../ScintillaEditBase/ScintillaQt.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'ScintillaQt.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.14.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
QT_WARNING_PUSH
QT_WARNING_DISABLE_DEPRECATED
struct qt_meta_stringdata_Scintilla__ScintillaQt_t {
    QByteArrayData data[26];
    char stringdata0[280];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    qptrdiff(offsetof(qt_meta_stringdata_Scintilla__ScintillaQt_t, stringdata0) + ofs \
        - idx * sizeof(QByteArrayData)) \
    )
static const qt_meta_stringdata_Scintilla__ScintillaQt_t qt_meta_stringdata_Scintilla__ScintillaQt = {
    {
QT_MOC_LITERAL(0, 0, 22), // "Scintilla::ScintillaQt"
QT_MOC_LITERAL(1, 23, 18), // "horizontalScrolled"
QT_MOC_LITERAL(2, 42, 0), // ""
QT_MOC_LITERAL(3, 43, 5), // "value"
QT_MOC_LITERAL(4, 49, 16), // "verticalScrolled"
QT_MOC_LITERAL(5, 66, 22), // "horizontalRangeChanged"
QT_MOC_LITERAL(6, 89, 3), // "max"
QT_MOC_LITERAL(7, 93, 4), // "page"
QT_MOC_LITERAL(8, 98, 20), // "verticalRangeChanged"
QT_MOC_LITERAL(9, 119, 12), // "notifyParent"
QT_MOC_LITERAL(10, 132, 14), // "SCNotification"
QT_MOC_LITERAL(11, 147, 3), // "scn"
QT_MOC_LITERAL(12, 151, 12), // "notifyChange"
QT_MOC_LITERAL(13, 164, 11), // "aboutToCopy"
QT_MOC_LITERAL(14, 176, 10), // "QMimeData*"
QT_MOC_LITERAL(15, 187, 4), // "data"
QT_MOC_LITERAL(16, 192, 7), // "command"
QT_MOC_LITERAL(17, 200, 6), // "uptr_t"
QT_MOC_LITERAL(18, 207, 6), // "wParam"
QT_MOC_LITERAL(19, 214, 6), // "sptr_t"
QT_MOC_LITERAL(20, 221, 6), // "lParam"
QT_MOC_LITERAL(21, 228, 6), // "onIdle"
QT_MOC_LITERAL(22, 235, 11), // "execCommand"
QT_MOC_LITERAL(23, 247, 8), // "QAction*"
QT_MOC_LITERAL(24, 256, 6), // "action"
QT_MOC_LITERAL(25, 263, 16) // "SelectionChanged"

    },
    "Scintilla::ScintillaQt\0horizontalScrolled\0"
    "\0value\0verticalScrolled\0horizontalRangeChanged\0"
    "max\0page\0verticalRangeChanged\0"
    "notifyParent\0SCNotification\0scn\0"
    "notifyChange\0aboutToCopy\0QMimeData*\0"
    "data\0command\0uptr_t\0wParam\0sptr_t\0"
    "lParam\0onIdle\0execCommand\0QAction*\0"
    "action\0SelectionChanged"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_Scintilla__ScintillaQt[] = {

 // content:
       8,       // revision
       0,       // classname
       0,    0, // classinfo
      11,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       8,       // signalCount

 // signals: name, argc, parameters, tag, flags
       1,    1,   69,    2, 0x06 /* Public */,
       4,    1,   72,    2, 0x06 /* Public */,
       5,    2,   75,    2, 0x06 /* Public */,
       8,    2,   80,    2, 0x06 /* Public */,
       9,    1,   85,    2, 0x06 /* Public */,
      12,    0,   88,    2, 0x06 /* Public */,
      13,    1,   89,    2, 0x06 /* Public */,
      16,    2,   92,    2, 0x06 /* Public */,

 // slots: name, argc, parameters, tag, flags
      21,    0,   97,    2, 0x08 /* Private */,
      22,    1,   98,    2, 0x08 /* Private */,
      25,    0,  101,    2, 0x08 /* Private */,

 // signals: parameters
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int, QMetaType::Int,    6,    7,
    QMetaType::Void, QMetaType::Int, QMetaType::Int,    6,    7,
    QMetaType::Void, 0x80000000 | 10,   11,
    QMetaType::Void,
    QMetaType::Void, 0x80000000 | 14,   15,
    QMetaType::Void, 0x80000000 | 17, 0x80000000 | 19,   18,   20,

 // slots: parameters
    QMetaType::Void,
    QMetaType::Void, 0x80000000 | 23,   24,
    QMetaType::Void,

       0        // eod
};

void Scintilla::ScintillaQt::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        auto *_t = static_cast<ScintillaQt *>(_o);
        Q_UNUSED(_t)
        switch (_id) {
        case 0: _t->horizontalScrolled((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: _t->verticalScrolled((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: _t->horizontalRangeChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 3: _t->verticalRangeChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 4: _t->notifyParent((*reinterpret_cast< SCNotification(*)>(_a[1]))); break;
        case 5: _t->notifyChange(); break;
        case 6: _t->aboutToCopy((*reinterpret_cast< QMimeData*(*)>(_a[1]))); break;
        case 7: _t->command((*reinterpret_cast< uptr_t(*)>(_a[1])),(*reinterpret_cast< sptr_t(*)>(_a[2]))); break;
        case 8: _t->onIdle(); break;
        case 9: _t->execCommand((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 10: _t->SelectionChanged(); break;
        default: ;
        }
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        switch (_id) {
        default: *reinterpret_cast<int*>(_a[0]) = -1; break;
        case 9:
            switch (*reinterpret_cast<int*>(_a[1])) {
            default: *reinterpret_cast<int*>(_a[0]) = -1; break;
            case 0:
                *reinterpret_cast<int*>(_a[0]) = qRegisterMetaType< QAction* >(); break;
            }
            break;
        }
    } else if (_c == QMetaObject::IndexOfMethod) {
        int *result = reinterpret_cast<int *>(_a[0]);
        {
            using _t = void (ScintillaQt::*)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::horizontalScrolled)) {
                *result = 0;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::verticalScrolled)) {
                *result = 1;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(int , int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::horizontalRangeChanged)) {
                *result = 2;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(int , int );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::verticalRangeChanged)) {
                *result = 3;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(SCNotification );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::notifyParent)) {
                *result = 4;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)();
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::notifyChange)) {
                *result = 5;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(QMimeData * );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::aboutToCopy)) {
                *result = 6;
                return;
            }
        }
        {
            using _t = void (ScintillaQt::*)(uptr_t , sptr_t );
            if (*reinterpret_cast<_t *>(_a[1]) == static_cast<_t>(&ScintillaQt::command)) {
                *result = 7;
                return;
            }
        }
    }
}

QT_INIT_METAOBJECT const QMetaObject Scintilla::ScintillaQt::staticMetaObject = { {
    QMetaObject::SuperData::link<QObject::staticMetaObject>(),
    qt_meta_stringdata_Scintilla__ScintillaQt.data,
    qt_meta_data_Scintilla__ScintillaQt,
    qt_static_metacall,
    nullptr,
    nullptr
} };


const QMetaObject *Scintilla::ScintillaQt::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *Scintilla::ScintillaQt::qt_metacast(const char *_clname)
{
    if (!_clname) return nullptr;
    if (!strcmp(_clname, qt_meta_stringdata_Scintilla__ScintillaQt.stringdata0))
        return static_cast<void*>(this);
    if (!strcmp(_clname, "ScintillaBase"))
        return static_cast< ScintillaBase*>(this);
    return QObject::qt_metacast(_clname);
}

int Scintilla::ScintillaQt::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 11)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 11;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 11)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 11;
    }
    return _id;
}

// SIGNAL 0
void Scintilla::ScintillaQt::horizontalScrolled(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Scintilla::ScintillaQt::verticalScrolled(int _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Scintilla::ScintillaQt::horizontalRangeChanged(int _t1, int _t2)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t2))) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Scintilla::ScintillaQt::verticalRangeChanged(int _t1, int _t2)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t2))) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void Scintilla::ScintillaQt::notifyParent(SCNotification _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void Scintilla::ScintillaQt::notifyChange()
{
    QMetaObject::activate(this, &staticMetaObject, 5, nullptr);
}

// SIGNAL 6
void Scintilla::ScintillaQt::aboutToCopy(QMimeData * _t1)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void Scintilla::ScintillaQt::command(uptr_t _t1, sptr_t _t2)
{
    void *_a[] = { nullptr, const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t1))), const_cast<void*>(reinterpret_cast<const void*>(std::addressof(_t2))) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}
QT_WARNING_POP
QT_END_MOC_NAMESPACE
