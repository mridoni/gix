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
