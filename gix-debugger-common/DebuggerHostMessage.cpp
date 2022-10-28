#include "DebuggerHostMessage.h"

void DebuggerHostMessage::set_value(any_type& a, json11::Json v)
{
	std::string d = v.dump();

	switch (v.type()) {
	case json11::Json::Type::STRING:
		a = v.string_value();
		break;

	case json11::Json::Type::NUMBER:
		a = (int)v.number_value();
		break;

	case json11::Json::Type::BOOL:
		a = v.bool_value();
		break;

	case json11::Json::Type::ARRAY:
	{
		auto arr = v.array_items();

		std::vector<std::string> vec;

		for (auto it = arr.begin(); it != arr.end(); ++it) {
			vec.push_back(it->string_value());
		}
		a = vec;

	}
	break;

	case json11::Json::Type::OBJECT:
	{
		auto oo = v.object_items();
		if (oo.find("__obj_type__") != oo.end() && oo["__obj_type__"] == "GixDebuggerSessionConfig") {
			GixDebuggerSessionConfig cfg;

			cfg.setHostLocalLogLevel(oo["dbgr_host_local_log_level"].number_value());
			cfg.setHostLocalLogFile(oo["dbgr_host_local_log_file"].string_value());
			cfg.setHostRemoteLogLevel(oo["dbgr_host_remote_log_level"].number_value());
			cfg.setProgram(oo["program"].string_value());
			cfg.setWorkingDirectory(oo["working_dir"].string_value());
			cfg.setModuleDirectory(oo["module_dir"].string_value());
			cfg.setDebuggedModuleType((DebuggedModuleType)oo["module_type"].number_value());
			cfg.setUseExternalConsole(oo["use_external_console"].bool_value());
			//cfg.setUseHostExternalConsole(oo["use_host_external_console"].bool_value());
			cfg.setStdInFile(oo["use_host_external_console"].string_value());
			cfg.setHostSessionType((DebuggerSessionType)oo["dbgr_host_session_type"].number_value());
			cfg.setPid(oo["pid"].number_value());

			any_type pa;
			set_value(pa, oo["program_args"]);
			cfg.setProgramArgs(pa.toStringVector());

			any_type env;
			set_value(env, oo["environment"]);
			cfg.setEnvironment(env.toStringMap());

			any_type hp;
			set_value(hp, oo["dbgr_host_properties"]);
			cfg.setHostProperties(hp.toStringMap());

			a = cfg;
		}
		else
		{
			if (oo.find("__obj_type__") != oo.end() && oo["__obj_type__"] == "VariableDisplayData") {
				VariableDisplayData vdd;
				vdd.var_name = oo["var_name"].string_value();
				vdd.module_name = oo["module_name"].string_value();
				vdd.var_path = oo["var_path"].string_value();
				vdd.type = (WsEntryType)oo["type"].number_value();
				vdd.level = oo["level"].number_value();
				vdd.display_size = oo["display_size"].number_value();
				vdd.is_signed = oo["is_signed"].bool_value();
				vdd.decimals = oo["decimals"].number_value();
				vdd.format = oo["format"].string_value();
				vdd.storage_type = oo["storage_type"].number_value();
				vdd.storage = oo["storage"].string_value();
				vdd.occurs = oo["occurs"].number_value();
				vdd.redefines = oo["redefines"].string_value();
				vdd.base_var_name = oo["base_var_name"].string_value();
				vdd.local_sym_name = oo["local_sym_name"].string_value();
				vdd.local_addr = oo["local_addr"].number_value();
				vdd.storage_size = oo["storage_size"].number_value();
				vdd.display_data = oo["display_data"].string_value();

				a = vdd;
			}
			else
			{
				if (oo.size() == 0 || oo.begin()->second.is_string()) {
					std::map<std::string, std::string> m;

					for (auto it = oo.begin(); it != oo.end(); ++it) {
						m[it->first] = it->second.string_value();   // only std::string, std::string>, no need to check
					}
					a = m;
				}
				else {
					std::map<std::string, VariableDisplayData> vec;

					for (auto it = oo.begin(); it != oo.end(); ++it) {
						any_type vdd;
						set_value(vdd, it->second);
						VariableDisplayData vd = vdd.toVariableDisplayData();
						vec[vd.var_name] = vd;
					}
					a = vec;
				}
			}
		}
	}
	break;

	case json11::Json::Type::NUL:
		break;
	}

}