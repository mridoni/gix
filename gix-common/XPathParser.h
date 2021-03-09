/*
Gix-IDE, an IDE and platform for GnuCOBOL
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

#pragma once

#include <string>
#include <vector>
#import <msxml6.dll>


namespace XPathNS // XPath Namespace
{
	struct XMLAttribute
	{
		std::string value_;
		std::string name_;
	};

	struct XMLNode
	{
		std::string xml_;
		std::string value_;
		std::string name_;
		std::vector<XMLAttribute> nodeAttributes_;
	};

	class XPathParser
	{
		MSXML2::IXMLDOMDocumentPtr pXMLDoc_; 
		std::vector<XMLNode> nodeList_;
	public:
		XPathParser( std::string xmlFile );
		~XPathParser(void);

		XMLNode selectSingleNode( std::string xPathExpression );
		std::vector<XMLNode> selectNodes( std::string xPathExpression );
	};

}
