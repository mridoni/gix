/*
This file is part of Gix-IDE, an IDE and platform for GnuCOBOL
Copyright (C) 2021 Marco Ridoni

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
*/

#ifndef CUSTOMDIALOG_H
#define CUSTOMDIALOG_H

//############################################################
//  
//  This file contains a special class, "CustomDialog", for generating
//  a modal dialog box with multiple form elements/inputs including:
//  labels, checkboxes, line edit boxes, number edit boxes, combo boxes,
//  radio buttons, spin boxes (for ints or floats), group boxes (to group elements)
//  and even "pick color" buttons (letting you chose a color).
//  
//  NOTES:
//    
//    > Each form element is created by a single function only, includes an is_optional tooltip,
//      and a pointer to the variable you wish to modify.
//      
//    > The value of the variable is updated ONLY if and when hits "Ok", and will change only
//      if the user has changed it from the original/default/starting value in that variable.
//
//    > This class if fantastic for making modal dialogs (i.e. dialogs where you can't click 
//      anything else until you click okay or cancel), but isn't designed for times when you 
//      want/need the parent dialog to dymaically update as the user changes values
//       (i.e. interactive changes).
//    
//    > This method of generating dialog elements on-the-fly by using a vectors of structs is 
//      obviously less versatile than creating a seperate class which extend QDialog for EACH
//      dialog you need. The huge advantage, however, is you can can greatly reduce your code 
//      and avoid the tedium of creating a seperate .h and .cpp file everytime you wants to 
//      prompt the user for multiple pieces of info in a single modal dialog!
//    
//    > This file also includes several "convinience" functions such as "MsgBoxYesNo" and 
//      "setBold" for quickly displaying dialogs/returning results and modifying strings.
//      These convinience functions are at the bottom of this file (and defined in the .cpp).
//      
//  -----------------
//  
//  An example of how to use this dialog follows:
//  
//    void GetDetailsFromUser()
//    {
//      std::string  name     = "First Last";
//      bool    student  = true;
//      int     age      = 20;
//      int     sportIdx = 1;
//      
//      CustomDialog d("Registration", this);
//      d.addLabel    ("Please enter your details below ...");
//      d.addLineEdit ("name:  ", &name, "No middle name!");
//      d.addCheckBox ("current student", &student);
//      d.addSpinBox  ("your age: ", 1, 120, &age, 1);
//      d.addComboBox ("sport: ", "tennis|soccer|cricket", &sportIdx);
//      
//      d.exec();                    // Execution stops here until user closes dialog
//      
//      if(d.wasCancelled()) {
//        cout << "You have cancelled your registration" << endl;
//        return;
//      }
//      
//      cout << "Dear " << name << " thank you for registering..." << endl;
//      
//      ... CODE ...
//    }
//  
//  -----------------
//  
//  And produces a dialog which looks like this:
//  
//    +----------------------------------------+
//    | Registration                       ? X |
//    +----------------------------------------+
//    | Please enter your details below ...    |
//    |        _____________________________   |
//    | name: |__First_Last_________________|  |
//    |                                        |
//    | [X] current student                    |
//    |                                _____   |
//    | your age:                     [_22_^]  |
//    |                       ______________   |
//    | sport:               |_soccer_____[V]  |
//    |                                        |
//    |   +-------------+    +-------------+   |
//    |   |     Ok      |    |   Cancel    |   |
//    |   +-------------+    +-------------+   |
//    +----------------------------------------+
//  
//
// 
//  > author:       Andrew Noske
//  > last updated: 6-June-2012
// 
// http://www.andrewnoske.com/wiki/index.php?title=Code_-_qt_custom_input_dialog
//############################################################


#include <iostream>
#include <vector>
#include <string>
#include <functional>

#include <qdialog.h>
#include <qobject.h>
#include <qvariant.h>
#include <qaction.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtextedit.h>
#include <qprogressbar.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
#include <qcompleter.h>
#include <QHBoxLayout>
#include <QLabel>
#include <QVBoxLayout>
#include <QListWidget>

#include <QDesktopServices>
#include <QDir>
#include <QUrl>

//############################################################

//## CONSTANTS:

enum DlgType {
	DLG_LABEL, DLG_CHECKBOX, DLG_LINEEDIT, DLG_FLOATEDIT,
	DLG_SPINBOX, DLG_DBLSPINBOX, DLG_MINMAXSPIN,
	DLG_COMBOBOX, DLG_RADIOGRP, DLG_GRPBOX, DLG_COLOR, DLG_TEXTEDIT, DGL_ALL,
	DLG_EDITABLE_PATH_LIST, DLG_EDITABLE_STRING_LIST, DLG_GET_FOLDER
};

enum chkbehav { CB_NONE, CB_DISABLE, CB_ENABLE, CB_HIDE, CB_SHOW };

enum btnset { BS_CANCEL_OKAY, BS_OKAY_ONLY, BS_NO_YES, BS_CUSTOM };

enum btnbehav { BB_ACCEPT, BB_REJECT, BB_DISABLED, BB_POPUP, BB_OPENURL, BB_ACCEPT2, BB_ACCEPT3 };

//############################################################

//## SMALL CLASSES:

class ColorButton : public QPushButton    // Used to create a "pick color" button.
{
	Q_OBJECT      // NOTE: To compile on my version of IMOD I have to comment these out.

public:
	QColor color;
	ColorButton(QColor _color, QWidget *parent = 0);
	void setColor(QColor _color);
	QColor getColor();
public slots:
	void pickColor();
	//public signals:
	//  void valueChanged() {};
};

//############################################################

//************************
//** DialogElement is used to store gui widgets in the array of 
//** widgets displayed in GuiDialogCustomizable

struct DialogElement
{
	DlgType type;                 // The "type" of dialog element displayed this row.
	bool    extraChkAdded;        // Set true if a special extra checkbox is added.
								  //  using CustomDialog.addCheckPrev()

								  //** POINTERS USE TO PASS BACK ANY CHANGED VALUES:

	std::string  *returnString;        // For DLG_LINEEDIT.
	QStringList *returnStringList;
	int     *returnInt;           // For DLG_SPINBOX, DLG_COMBOBOX & DLG_RADIOGRP.
	int     *returnInt2;          // For DLG_DBLSPINBOX.
	bool    *returnBool;          // For DLG_CHECKBOX.
	float   *returnFloat;         // For DLG_FLOATEDIT & DLG_DBLSPINBOX.
	QColor  *returnColor;         // For DLG_COLOR.
	bool    *returnChkExtra;      // Fsed if extraChkAdded is true.

	bool readOnly;                // if set to true, user cannot change the text.

								  //** FORM ELEMENTS TO DISPLAY (DEPENDING ON TYPE):

	QWidget        *wid;
	QHBoxLayout    *layout;

	QLabel         *label;
	QLabel         *label2;
	QCheckBox      *chkBox;
	QLineEdit      *lineEdit;
	QSpinBox       *spnBox;
	QSpinBox       *spnBox2;
	QDoubleSpinBox *dblSpnBox;
	QComboBox      *cmbBox;
	ColorButton    *btnColor;
	std::vector<QRadioButton*> radBtn;
	QGroupBox      *grpBox;
	QTextEdit      *textEdit;

	QCheckBox      *chkExtra;

	QListWidget	   *lstStrings;
};

//############################################################

class CustomDialog;
typedef std::function<bool(CustomDialog *)> f_validate_callback;

//************************
//** GuiDialogCustomizable is used to present a customizable gui
//** dialog and retrieve user input with minimal code!

class CustomDialog : public QDialog
{
	Q_OBJECT

public:     //## METHODS:

	CustomDialog(QString title, QWidget *parent = 0, btnset = BS_CANCEL_OKAY);
	~CustomDialog() {};
	bool setDialogElements();
	bool wasCancelled();

	bool addCustomButton(QString buttonStr, btnbehav buttonBehav = BB_ACCEPT, QString tooltip = "");


	DialogElement& addNewElement(DlgType _type, QString caption, QString tooltip, bool makeLabel);
	int addLabel(QString caption, bool bold = false, QString tooltip = "");
	int addHtmlLabel(QString caption, QString tooltip = "");
	int addCheckBox(QString caption, bool *checked, QString tooltip = "");
	int addLineEdit(QString caption, std::string *stringValue, QString tooltip = "");
	int addReadOnlyLineEdit(QString caption, QString text, QString tooltip = "");
	int addLineEditF(QString caption, float min, float max, float *value, float decimals, QString tooltip = "", QString unitsStr = "");
	int addSpinBox(QString caption, int min, int max, int *value, int step, QString tooltip = "");
	int addDblSpinBoxF(QString caption, float min, float max, float *value, int decimals, float step = 0.1, QString tooltip = "");
	int addComboBox(QString caption, QString barSepList, int *selIdx, QString tooltip = "");
	int addRadioGrp(QString caption, QString barSepList, int *selIdx, QString tooltip = "", QString tooltipArr = "", bool checkable = false, bool *checked = 0);
	int addColorSel(QString caption, QColor *color, QString tooltip = "");
	int addMinMaxSpinBoxPair(QString caption, QString middleCaption, int min, int max, int *minValue, int *maxValue, int step = 1, QString tooltip = "");
	int addTextEdit(std::string *text, bool richText, bool readOnly, int minHeight = 90, QString tooltip = "");
	int addReadOnlyTextEdit(QString text, bool richText, int minHeight = 90, QString tooltip = "");
	int addProgressBar(QString caption, int percent, int width, bool showValue, QString tooltip = "");
	int addPercentBar(QString caption, QString valueLabel, float percent, int width, QColor colorBar, QString tooltip = "", QFrame::Shape shape = QFrame::StyledPanel, QFrame::Shadow shadow = QFrame::Sunken);
	int addVSpacer(int minHeight = 0);
	int addEditablePathList(QString caption, QString tooltip, QStringList *pathList);
	int addEditableStringList(QString caption, QString tooltip, QStringList *stringList);
    int addGetFolder(QString caption, std::string *folder_path, std::string default_path = std::string());

	int beginGroupBox(QString caption, bool flat = false, QString tooltip = "", bool checkable = false, bool *checked = 0);
	void endGroupBox();

	int addCheckPrev(QString caption, bool *checked, chkbehav chkBeh, bool removeLabel, QString tooltip = "");
	int addAutoCompletePrev(QStringList wordList, bool caseSensitive = false);
	bool setStyleElem(int idx, std::string styleStr, bool bold = false);
	void setStylePrev(std::string styleStr, bool bold = false);

	bool setEnabledElem(int idx, bool enabled);
	void setEnabledPrev(bool enabled);
	void setEnabledAll(bool enabled);

	void setValidationCallback(f_validate_callback vb) { validate_callback = vb; }


public:       //## DATA:

	std::vector<DialogElement> elements;     // The std::vector of GUI elements used to display
										//   and change the values.
	int customBtnClicked;               // Set to the index of the button
										//   "customBtn" clicked.
	
	f_validate_callback validate_callback = nullptr;
private:


	std::vector<QPushButton*> customBtn;      // std::vector of buttons down the button of the GUI.
	QVBoxLayout *vboxLayout;
	QHBoxLayout *hbtnLayout;

	bool addToGroupBox;
	QVBoxLayout *groupBoxLayout;
	QVBoxLayout *layoutNextElement;

	void addPathToPathList(DialogElement &e);
	void removePathFromPathList(DialogElement &e);

	void addVarToVarList(DialogElement &e);
	void removeVarFromVarList(DialogElement &e);

public slots:   //## SLOTS:

	void customBtnAccept();
	void customBtnReject();
	void customBtnMessage();
	void customBtnOpenUrl();
	void updateBtnClicked(QObject *btnClicked);
	void resizeMe();
	int exec();
};


//############################################################


//-------------------------------
//## SMALL MESSAGE BOX FUNCTIONS:

void MsgBox(std::string str);
void MsgBox(QWidget *parent, QString title, QString str);
bool MsgBoxYesNo(QWidget *parent, std::string str);
std::string InputBoxString(QWidget *parent, std::string title, std::string label, std::string defaultStr);

//-------------------------------
//## SMALL INLINE GUI FUNCTIONS:

inline QString QStr(int number);
inline QString QStr(long number);
inline QString QStr(float number);
inline QString QStr(double number);
inline std::string qStringToString(QString qstr);
inline QString nbsp(int numSpaces);
inline void setBold(QWidget *wid, bool bold);
inline void setTextColor(QWidget *wid, int r, int g, int b);
inline void setDefaultColorAndFont(QWidget *wid);
inline void openUrl(QString urlString, bool addFilePrefix = false);

//############################################################

//----------------------------------------------------------------------------
//
//          SMALL INLINE GUI FUNCTIONS:
//
//----------------------------------------------------------------------------




//---------
//-- Short function name for converting numbers to a QString.

inline QString QStr(int number) { return QString::number(number); }
inline QString QStr(long number) { return QString::number(number); }
inline QString QStr(float number) { return QString::number(number); }
inline QString QStr(double number) { return QString::number(number); }

//---------
//-- Converts a QString to a standard std::string

inline std::string qStringToString(QString qstr)
{
	std::string str = "";
	for (int i = 0; i<qstr.length(); i++)
		str += qstr.at(i).toLatin1();
	return str;
}

//---------
//-- Creates a qstring with the specified number
//-- of "non breaking space" HTML characters

inline QString nbsp(int numSpaces)
{
	QString str = "";
	for (int i = 0; i<numSpaces; i++)
		str += "&nbsp;";
	return str;
}

//---------
//-- Short function name for taking any widget and making the text in it bold.

inline void setBold(QWidget *wid, bool bold)
{
	QFont font;
	font.setBold(bold);
	wid->setFont(font);
}

//---------
//-- Short function name for setting the text (forground) color of a widget.

inline void setTextColor(QWidget *wid, int r, int g, int b)
{
	wid->setStyleSheet("color: rgb(" + QStr(r) + "," + QStr(g) + "," + QStr(b) + ");");
}


//---------
//-- Short function which sets the font to default, the foreground/text color to black
//-- and background to transparent. This function is useful for when you might apply a
//-- stylesheet to a container object, but you don't want those changes to apply to 
//-- (heirarchially) to widgets within it.

inline void setDefaultColorAndFont(QWidget *wid)
{
	wid->setFont(QFont());
	wid->setStyleSheet("color: rgb(0, 0, 0); background-color: rgba(255, 255, 255, 0);");
}

//---------
//-- Opens the given URL ("urlString") in the default web browser.
//-- If ("addFilePrefix") is true, the std::string "file://" is added to the
//-- front and, in most operating systems (including OSX), this will
//-- mean the specified file on the computer should be opened
//-- in the default program (eg: a .xls file open in Excel) instead.

inline void openUrl(QString urlString, bool addFilePrefix)
{
	if (addFilePrefix)
		urlString = "file://" + urlString;
	QDesktopServices::openUrl(QUrl(urlString));
}

#endif
