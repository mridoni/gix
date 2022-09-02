## Gix-IDE and Web Modules

*Note: the "Web Modules" feature is still in its infancy: while working for common cases, it might break under load and/or expose quirky behaviour here and there. Moreover, data sanitization and verification are only minimally implemented (for now, obviously) so particular care should be taken not to expose or leak sensitive data by accident.  
Needless to say, **this should not be currently used on a public facing server without a properly configured firewall in between**.*

Gix-IDE includes a HTTP server (gix-http, based on [QtWebApp](http://stefanfrings.de/qtwebapp/index-en.html) by Stefan Frings) that allows you to expose GnuCOBOL modules as web services (currently REST only).

To build this kind of modules, you have to create a new Web/WebService project in Gix-IDE. The IDE will automatically set up a project that can be compiled and launched.
Practically any COBOL source can be compiled as a web module, there are only a few constraints that must be considered:

 - Web modules are always compiled as DLLs (dynamically loadable
   modules). 
  - You have to setup a COPY file in the project that will have to be
   included in the LINKAGE-SECTION. This will be used by gix-http to
   correctly to map the input/output from your program. Depending on
   your needs you can use a single COPY file that contains both input and output fields or separate COPY files. All of this can be setup from the IDE.
  - Web modules must be compiled within the IDE: you cannot just use any binary GnuCOBOL module, since Gix-IDE, when building the module, automatically adds some metadata that will be used at runtime to map the input/output areas to the parameters used in the HTTP calls.

### How it works
When gix-http starts, it parses a configuration file that contains information on the server itself (e.g. the HTTP port) and on the services it will make available. Each service corresponds to a Web Module and, in turn, to a COBOL "main" program. The "main" program can call other sub-programs as needed. For now, only REST services are supported.

### How to start
Open Gix-IDE and create a new Project Collection, then set the options like this (you can check "Precompile for ESQL" if needed):  
![New Web Project](https://www.mediumgray.info/img/new_webprj.png)

A new Web Project will be created, containing a single module and a COPY file:  
![Web Project structure](https://www.mediumgray.info/img/webprj_prjc.png)

The sample module contains the following COBOL code:

```cobol
       IDENTIFICATION DIVISION.
       
       PROGRAM-ID. WEBTEST002.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       WORKING-STORAGE SECTION.

            01  AA      PIC X(255).
            01  BB      PIC X(255).
            
       LINKAGE SECTION.
            
         COPY IOWEBTEST002.
            
       PROCEDURE DIVISION
          USING PAR-IN, PAR-OUT.
          
           DISPLAY 'GOT CALL FROM CLIENT'.
           
           MOVE OPCODE-IN TO OPCODE-OUT.
           MOVE FUNCTION CURRENT-DATE TO DATA-OUT-1.
```


As you can see, there is a sample copy file (in this case IOWEBTEST002) that is included in the `LINKAGE-SECTION` area. The copy file has this structure:

```cobol
        01  PAR-IN.
          03  OPCODE-IN      PIC X(2).
          03  DATA-IN.
                 05  DATA-IN-1 PIC X(64).
                 05  DATA-IN-2 PIC X(64).
			  
        01  PAR-OUT.
          03  OPCODE-OUT      PIC X(2).
          03  DATA-OUT.
              05  DATA-OUT-1 PIC X(64).
              05  DATA-OUT-2 PIC X(64).            
```

`PAR-IN` and `PAR-OUT` (and their child fields) will be mapped at runtime when an HTTP call is performed. This mapping is already set by default in the new project, but obviously you cam completely change the name of the COPY files and of the input/output data areas. To do this, select the module file (WEBTEST002.cbl) in the Project Collection window, and go to the Properties window. 

There, you will find the "Expose as REST Web Service" option, already set to Yes. This indicates that this module will be used as a Web Module, i.e. the appropriate metadata will be generated to allow gix-http to use it as an entry point. You can have other modules in the same project, called in turn by the Web (entry) module, but that cannot serve as Web modules themselves.  
![REST Web Service](https://www.mediumgray.info/img/rest_opts_click.png)

Click on the "..." button and a dialog will appear, where you can set some runtime options:  
![REST Web Service options](https://www.mediumgray.info/img/rest_opts.png)

For the time being, and for the sake of this tutorial, leave all the options like you found them.

Now run the project by clicking the "Run" button on the toolbar, your service will be started:  
![Web project starting](https://www.mediumgray.info/img/webprj_start_log.png)


As you can see from the log the input/output area definitions are parsed and the service (named WEBTEST002, just like your project, is started.

You can also note that the log contains two URLs: the first must be used to call the service itself. The second one (the "schema URL"), if called will output a "schema" of the input/data you can supply/receive to/from the service.

Both the schema definition and the output data are in JSON format, while the input data (for now) is supplied through GET/POST parameters (this mechanism will be extended in the future to allow JSON data as input).

Let's call the schema URL, which in this case is `http://localhost:9090/WEBTEST002/schema`  
![Schema call output](https://www.mediumgray.info/img/websvc_schema.png)
 
 As you can see, we get a JSON object whose two properties `properties_in` and `properties_out` contain the data definitions for input/output areas.

Now we can try to call the service from a web browser (or with curl/wget) passing some parameters in the URL (we'll need to replace the hyphens in field names with underscores). So if we call the URL `http://localhost:9090/WEBTEST002?OPCODE_IN=73` we get:  
![Web service call data](https://www.mediumgray.info/img/websvc_data.png)
