AUTOMAKE_OPTIONS = subdir-objects

EJABBERD_PREFIX=$(libdir)/ejabberd

ESRCDIR=$(top_srcdir)/src
EBINDIR=$(top_builddir)/ebin

ERL=erl $(ERLCFLAGS) -pa ebin $(ERLFLAGS) -noshell -eval

mod_scribe_FILES = ejabberd_auth_couchdb.erl auth_couchdb_test.erl 
mod_scribe_BEAMS = ejabberd_auth_couchdb.beam auth_couchdb_test.beam

EXTRA_DIST = ${addprefix $(ESRCDIR)/, $(mod_scribe_FILES)}

mod_scribedir = $(EJABBERD_PREFIX)/ebin
mod_scribe_DATA = ${addprefix $(EBINDIR)/, $(mod_scribe_BEAMS)}
CLEANFILES = $(mod_scribe_DATA)

test: all
	$(ERL) 'auth_couchdb_test:test(), halt().'

$(EBINDIR)/%.beam: $(ESRCDIR)/%.erl
	$(ERLC) $(ERLCFLAGS) -b beam -o $(EBINDIR) $<
