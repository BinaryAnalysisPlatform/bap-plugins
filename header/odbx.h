// odbx.cpp

// Stmt Conn:create(const char* sql, Stmt::Type type)
void _ZN7OpenDBX4Conn6createERKSsNS_4Stmt4TypeE(void *this, void *stmt, const char* sql, int type);

// TODO overloaded Conn:create:
// Stmt Conn::create( const char* sql, unsigned long length, Stmt::Type type )

// Result Stmt::execute()
void* _ZN7OpenDBX4Stmt7executeEv();

// string& Conn::escape( const string& from, string& to)
void* _ZN7OpenDBX4Conn6escapeERKSsRSs(const void* from, void* to);

// TODO overloaded Conn:escape:
// string& Conn::escape( const char*, unsigned long fromlen, string& to)

// string& std::string::append(std::string const&)
void _ZNSs6appendERKSs(void *this, const char *data);

// string& std::string::append(char const*, unsigned int)
void _ZNSs6appendEPKcj(void *this, const char *data, unsigned int);
