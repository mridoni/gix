//using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using System.Threading;

namespace gixsql_tests
{
    public class GixSqlTestDataSourceInfo
    {
        public string type;
        public string hostname;
        public string port;
        public string dbname;
        public string username;
        public string password;
        public string options;

        internal string BuildConnectionString(bool embed_auth, bool embed_port = true, bool embed_dbname = true)
        {
            string res = type + "://";
            if (embed_auth)
                res += username + "." + password + "@";

            res += hostname;

            if (embed_port)
                res += ":" + port;

            if (embed_dbname)
                res += "/" + dbname;

            if (!String.IsNullOrWhiteSpace(options))
                res += "?" + options;

            return res;
        }

        public bool ExecSQLScript(string client_type, string arch, string[] sql_blocks)
        {
            string sql_block = null;

            if (sql_blocks == null || sql_blocks.Length == 0)
                return true;

            string provider = TestMatrixDataProvider.GetClientProvider(client_type, arch);
            Assert.IsFalse(String.IsNullOrWhiteSpace(provider));

            try
            {
                DbProviderFactory factory = DbProviderFactories.GetFactory(provider);
                DbConnectionStringBuilder sb = factory.CreateConnectionStringBuilder();

                switch (client_type)
                {
                    case "oracle":
                        sb["Data Source"] = hostname + "/" + dbname;
                        sb["User Id"] = username;
                        sb["Password"] = password;
                        break;

                    case "pgsql":
                        sb["Host"] = hostname;
                        sb["Database"] = dbname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        sb["Port"] = Int32.Parse(port);
                        break;

                    case "mysql":
                        sb["Host"] = hostname;
                        sb["Database"] = dbname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        sb["Port"] = Int32.Parse(port);
                        break;

                    case "sqlite":
                        sb["Data Source"] = hostname;
                        //sb["User Id"] = username;
                        //sb["Password"] = password;
                        break;

                    case "odbc":
                        sb["DSN"] = hostname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        break;
                }

                using (DbConnection conn = factory.CreateConnection())
                {
                    conn.ConnectionString = sb.ConnectionString;
                    conn.Open();


                    int i = 0;

                    using (DbTransaction tx = conn.BeginTransaction())
                    {
                        for (i = 0; i < sql_blocks.Length; i++)
                        {
                            sql_block = sql_blocks[i];
                            using (DbCommand cmd = conn.CreateCommand())
                            {
                                cmd.Transaction = tx;
                                cmd.CommandText = sql_block;

                                cmd.ExecuteNonQuery();
                            }
                        }
                        tx.Commit();

                        return true;
                    }
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);

                return false;
            }
            finally
            {
                if (client_type == "sqlite")
                    System.Data.SQLite.SQLiteConnection.ClearAllPools();
            }
        }

        internal void Reset()
        {
            if (type != "sqlite")
                return;

            for (int i = 0; i < 10; i++)
            {
                try
                {
                    if (File.Exists(hostname))
                        File.Delete(hostname);

                    break;
                }
                catch (Exception)
                {
                    Thread.Sleep(1000);
                }
            }
            if (File.Exists(hostname))
                throw new Exception("Can't reset data source " + hostname);

        }

        public bool RunSQLStatement(string client_type, string arch, string sql, Dictionary<string, string> stmt_params, bool report_error = true)
        {
            if (String.IsNullOrWhiteSpace(sql))
                return false;

            string provider = TestMatrixDataProvider.GetClientProvider(client_type, arch);
            Assert.IsFalse(String.IsNullOrWhiteSpace(provider));

            try
            {
                DbProviderFactory factory = DbProviderFactories.GetFactory(provider);
                DbConnectionStringBuilder sb = factory.CreateConnectionStringBuilder();

                switch (client_type)
                {
                    case "oracle":
                        sb["Data Source"] = hostname + "/" + dbname;
                        sb["User Id"] = username;
                        sb["Password"] = password;
                        break;

                    case "pgsql":
                        sb["Host"] = hostname;
                        sb["Database"] = dbname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        if (!String.IsNullOrWhiteSpace(port))
                            sb["Port"] = Int32.Parse(port);
                        break;

                    case "mysql":
                        sb["Host"] = hostname;
                        sb["Database"] = dbname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        if (!String.IsNullOrWhiteSpace(port))
                            sb["Port"] = Int32.Parse(port);
                        break;

                    case "odbc":
                        sb["DSN"] = hostname;
                        sb["UserName"] = username;
                        sb["Password"] = password;
                        break;

                    case "sqlite":
                        sb["Data Source"] = hostname;
                        break;
                }

                using (DbConnection conn = factory.CreateConnection())
                {
                    conn.ConnectionString = sb.ConnectionString;
                    conn.Open();

                    using (DbTransaction tx = conn.BeginTransaction())
                    {
                        using (DbCommand cmd = conn.CreateCommand())
                        {
                            cmd.Transaction = tx;
                            cmd.CommandText = sql;

                            if (stmt_params != null && stmt_params.Count > 0)
                            {
                                foreach (var kve in stmt_params)
                                {

                                    var p = cmd.CreateParameter();
                                    if (client_type != "mysql" && client_type != "odbc" )
                                        p.ParameterName = kve.Key;

                                    string pt = kve.Value.Substring(0, 1);

                                    switch (pt)
                                    {
                                        case "#":
                                        case ">":
                                            p.Value = System.Convert.FromBase64String(kve.Value.Substring(1));
                                            break;

                                        case "$":
                                            p.Value = kve.Value.Substring(1);
                                            break;

                                        default:
                                            p.Value = (string)kve.Value;
                                            break;
                                    }

                                    cmd.Parameters.Add(p);
                                }
                            }

                            cmd.ExecuteNonQuery();
                            tx.Commit();

                            return true;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                if (report_error)
                    Console.WriteLine(ex.Message);

                return false;
            }
            finally
            {
                if (client_type == "sqlite")
                    System.Data.SQLite.SQLiteConnection.ClearAllPools();
            }
        }
    }
}
