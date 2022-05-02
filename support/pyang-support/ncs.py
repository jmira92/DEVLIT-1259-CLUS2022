"""ncs output plugin

Invoke with: pyang --plugindir ${NCS_DIR}/support/pyang-support -p \
    ${NCS_DIR}/support/dm -p ${NCS_DIR}/lib/pyang/modules/ -f ncs <file>

"""


import optparse

from pyang import plugin
from pyang import util
from pyang import grammar
from pyang import error
from pyang import xpath
from pyang.plugins import tailf as tailf_plugin

tailf = 'tailf-common'
smiv2 = 'ietf-yang-smiv2'

def pyang_plugin_init():
    plugin.register_plugin(NCSPlugin())

class NCSPlugin(plugin.PyangPlugin):
    def add_output_format(self, fmts):
        fmts['ncs'] = self
    def add_opts(self, optparser):
        optlist = [
            optparse.make_option("--ncs-config",
                                 dest="ncs_config",
                                 default=True,
                                 action="store_true",
                                 help="Augment config (this is the default)"),
            optparse.make_option("--ncs-device-type",
                                 dest="ncs_device_type",
                                 default="netconf",
                                 action="store",
                                 help="Use if we have a non netconf device"),
            optparse.make_option("--ncs-skip-template",
                                 dest="ncs_skip_template",
                                 default=False,
                                 action="store_true",
                                 help="Use if we want to not gen the template tree"),
            optparse.make_option("--ncs-skip-statistics",
                                 dest="ncs_skip_statistics",
                                 default=False,
                                 action="store_true",
                                 help="Use if we want to not gen the stats tree"),
            optparse.make_option("--ncs-skip-config",
                                 dest="ncs_skip_config",
                                 default=False,
                                 action="store_true",
                                 help="Use if we want to gen ned stats"),

            ]
        g = optparser.add_option_group("NCS output specific options")
        g.add_options(optlist)

    def pre_validate(self, ctx, modules):
        module = modules[0]
        self.mods = [module.arg] + [i.arg for i in module.search('include')]

    def emit(self, ctx, modules, fd):
        # cannot do ncs unless everything is ok for our module
        # and submodules

        # for m in modules:
        #     fd.write('// xmod ' + m.arg + '\n')
        # d = ctx.__dict__
        # for k,v in d.items():
        #     fd.write('// key ' + k + '\n')
        #     if (k == 'modules'):
        #         for m in v:
        #             fd.write('// mod ' + m[0] + '\n')
        #             fd.write('//   m[1] ' + m[1]  + '\n')

        for (epos, etag, eargs) in ctx.errors:
            if (epos.top.arg in self.mods and
                error.is_error(error.err_level(etag)) and
                etag not in ctx.opts.warnings):
                fatal("%s contains errors" % epos.top.arg)
        emit_module(ctx, modules[0], fd)

_top_keywords = ['leaf', 'leaf-list', 'container', 'list',
                 'choice', 'anyxml', 'notification']

top_groupings = []
inner_groupings = []

def keep_tailf_annotation(where, ctx, kw):

    if ((ctx.opts.ncs_device_type == 'cli-ned') or
        (ctx.opts.ncs_device_type == 'generic-ned')):
        if ((kw == 'callpoint') or
            (kw == 'set-hook')):
            return True

    if (kw in ['indexed-view',
               'auto-compact'] and where == 'template'):
        return False

    if (kw in ['code-name',
               'value-length',
               'id-value',
               'key-default',
               'display-when',
               'dependency',
               'cli-diff-dependency',
               'cli-preformatted',
               'non-strict-leafref',
               'junos-val-as-xml-tag',
               'junos-val-with-prev-xml-tag',
               'sort-order',
               'sort-priority',
               'snmp-mib-module-name',
               'snmp-oid',
               'snmp-row-status-column',
               'info',
               'info-html',
               'confirm-text',
               'suppress-echo',
               'alt-name',
               'indexed-view',
               'auto-compact',
               'step',
               'hidden'
               ]):
        #  symlink;

        return True
    if (where == 'live'):
        return False
    if (kw.find('cli-') == 0):
        return True
    if (kw.find('snmp-ned-') == 0):
        return True
    return False

def emit_module(ctx, module, fd):
    if module.arg in ['ietf-yang-types', 'ietf-inet-types']:
        ctx.ietf_type_module = True
    else:
        ctx.ietf_type_module = False

    if (module.keyword == 'submodule'):
        fd.write('submodule %s {\n' % module.arg)
        bto = module.search_one('belongs-to')
        module.display_when_mod = bto.arg
        fd.write('  belongs-to %s {\n' % module.search_one('belongs-to').arg)
        fd.write('    prefix %s;\n' % bto.search_one('prefix').arg)
        ctx.module_path_prefix = bto.search_one('prefix').arg
        fd.write('  }\n')
    else:
        module.display_when_mod = module.arg
        fd.write('module %s {\n' % module.arg)
        fd.write('  namespace "%s";\n' % module.search_one('namespace').arg)
        fd.write('  prefix "%s";\n' % module.search_one('prefix').arg)
        ctx.module_path_prefix =  module.search_one('prefix').arg
        fd.write('\n')

    yang_version = module.search_one('yang-version')
    if yang_version:
        fd.write('  yang-version %s;\n\n' % yang_version.arg)

    # import tailf-common, if not imported already

    x = module.search_one('import', 'tailf-common')
    if x is None:
        if 'tailf' in module.i_prefixes:
            tailf_prefix = mk_prefix(module, 'tailf')
        else:
            tailf_prefix = 'tailf'
        if not ctx.ietf_type_module:
            fd.write('  import tailf-common {\n')
            fd.write('    prefix %s;\n' % tailf_prefix);
            fd.write('  }\n')
    else:
        tailf_prefix = x.search_one('prefix').arg
    ctx.tailf_prefix = tailf_prefix

    # import tailf-ncs, if not imported already
    x = module.search_one('import', 'tailf-ncs')
    if x is None:
        if 'ncs' in module.i_prefixes:
            ncs_prefix = mk_prefix(module, 'ncs')
        else:
            ncs_prefix = 'ncs'
        if not ctx.ietf_type_module:
            fd.write('  import tailf-ncs {\n')
            fd.write('    prefix %s;\n' % ncs_prefix);
            fd.write('  }\n')
    else:
        ncs_prefix = x.search_one('prefix').arg
    ctx.ncs_prefix = ncs_prefix
    # print all non-data statements (typedefs, identitys toplevel groupings etc)
    data_stmts = []
    augments = []

    for s in module.substmts:
        if s.keyword in ['namespace', 'prefix', 'belongs-to', 'yang-version']:
            # already handled
            continue
        if s.keyword == 'uses':
            data_stmts.append(s)
        elif s.keyword in _top_keywords:
            data_stmts.append(s)
        elif s.keyword == 'augment':
            augments.append(s)
        else:
            if s.keyword == 'grouping':
                top_groupings.append(s.arg)
            emit_stmt(tailf_prefix, module, False, False,
                      ctx, s, fd, '  ', '  ',
                      False, False, False, 'toplevel')

    if ctx.ietf_type_module:
        fd.write('}\n')
        return

    ## Now augment all the notifications
    if ctx.opts.ncs_device_type == 'netconf':
        for s in data_stmts:
            if (s.keyword == 'notification'):
                fd.write('  augment /%s:devices/%s:device/%s:netconf-notifications/%s:received-notifications/%s:notification/%s:data {\n' % \
                             (ncs_prefix, ncs_prefix, ncs_prefix,
                              ncs_prefix, ncs_prefix, ncs_prefix))
                fd.write("     container %s {\n" % s.arg);
                fd.write("        presence \"\";\n");
                fd.write("        config false;\n");
                for ss in s.substmts:
                    emit_stmt(tailf_prefix, module, True, False, ctx, ss, fd,
                              '        ', '  ',
                              False, False, False, 'notification')
                fd.write('      }\n')
                fd.write('  }\n')

    if (ctx.opts.ncs_skip_config == False):
        # print all data statements in an augment
        fd.write('  augment /%s:devices/%s:device/%s:config {\n' % \
                     (ncs_prefix, ncs_prefix, ncs_prefix))

        for s in data_stmts:
            emit_stmt(tailf_prefix, module, True, False, ctx, s,
                      fd, '    ', '  ',
                      False, False, False, 'device', add_device_type=True)
        fd.write('  }\n')
    else:
        fd.write('\n  // Skip entire config tree due to --ncs-skip-config flag\n\n')
    if (ctx.opts.ncs_skip_template == False):
        fd.write('  augment /%s:devices/%s:template/%s:config {\n' % \
                     (ncs_prefix, ncs_prefix, ncs_prefix))
        for s in data_stmts:
            emit_stmt(tailf_prefix, module, True, False,
                      ctx, s, fd, '    ', '  ',
                      False, False, False, 'template')
        fd.write('  }\n')
    else:
        fd.write('\n  // Skip entire template tree due to --ncs-skip-template flag\n\n')

    if (ctx.opts.ncs_skip_statistics == False):
        # Now generate the live tree
        fd.write('  augment /%s:devices/%s:device/%s:live-status {\n' % \
                     (ncs_prefix, ncs_prefix, ncs_prefix))

        for s in data_stmts:
            emit_stmt(tailf_prefix, module, True, False,
                      ctx, s, fd, '    ', '  ',
                      False, False, False, 'live', add_device_type=True)
        fd.write('  }\n')
    else:
        fd.write('\n  // Skip entire stats tree due to --ncs-skip-statistics flag\n\n')

    ## and finally generate the augment in augment - but rather
    ## as patched augments
    for s in augments:
        t, name = augment_type(s)
        if t == 'notification':
            old = s.arg
            patch_augment_in_augment(ctx, s, 'notification')
            emit_stmt(tailf_prefix, module, True, True,
                      ctx, s, fd, '  ', '  ',
                      False, False, False, 'device', -1)
            s.arg = old
        elif t == 'rpc':
            old = s.arg
            # figure out which prefix (if any) is used for the rpc name
            first = s.arg.split("/", 1)[1]
            if first.find(":") == -1:
                prefix = ""
            else:
                [prefix, _ignore] = first.split(":", 1)
            patch_augment_in_augment(ctx, s, 'rpc', prefix+":", name)
            emit_stmt(tailf_prefix, module, True, True,
                      ctx, s, fd, '  ', '  ',
                      False, False, False, 'device', -1)
            s.arg = old
        else:
            if (ctx.opts.ncs_skip_config == False):
                old = s.arg
                patch_augment_in_augment(ctx, s, 'device')
                emit_stmt(tailf_prefix, module, True, True,
                          ctx, s, fd, '  ', '  ',
                          False, False, False, 'device', -1)
                s.arg = old
            if (ctx.opts.ncs_skip_statistics == False):
                old = s.arg
                patch_augment_in_augment(ctx, s, 'live')
                emit_stmt(tailf_prefix, module, True, True,
                          ctx, s, fd, '  ', '  ',
                          False, False, False, 'live', -1)
                s.arg = old

            if (ctx.opts.ncs_skip_template == False):
                old = s.arg
                patch_augment_in_augment(ctx, s, 'template')
                emit_stmt(tailf_prefix, module, True, True,
                          ctx, s, fd, '  ', '  ',
                          False, False, False, 'template', -1)
            s.arg = old

    fd.write('}\n')

def augment_type(s):
    p = s.i_target_node
    while p is not None and p.keyword not in ('notification', 'rpc'):
        p = p.parent
    if p is None:
        return 'data', ""
    return p.keyword, p.arg

def mk_prefix(module, s):
    i = 0
    prefix = s + str(i)
    while prefix in module.i_prefixes:
        i = i + 1
        prefix = s + str(i)
    return prefix

# NOTE: does not work if called for a random statement; proper
#       useage is to call this for the top-level node, and then
#       recurse.
def is_stats(stmt):
    if stmt.keyword == 'augment' and hasattr(stmt.i_target_node, 'i_config'):
        if stmt.i_target_node.i_config == False:
            return True
    for s in stmt.substmts:
        if (s.keyword == 'config' and s.arg == 'false'):
            return True
    return False


def has_mandatory(fd, is_config, s):
    if is_config:
        if (hasattr(s, 'i_config')):
            if s.i_config == False:
                return False
        else:
            return False
    else:
        return False
    if (s.keyword == 'leaf'):
        for l in s.substmts:
            ## Must expressions may imply that the top container
            ## must have an added presence statement, we cannot know.
            if (l.keyword == 'must'):
                return True
            if (l.keyword == 'mandatory') and (l.arg == 'true'):
                return True
    if ((s.keyword == 'list') or (s.keyword =='leaf-list')):
        for l in s.substmts:
            if (l.keyword == 'min-elements') and (l.arg != '0'):
                return True
    # FIXME, check the grouping
    if (s.keyword == 'uses'):
        return True
    if (s.keyword == 'container'):
        # Is it a presence container
        ispresence = False
        for l in s.substmts:
            if (l.keyword == 'presence'):
                ispresence = True
            if (l.keyword == 'must'):
                return True
        if (ispresence == False):
            for l in s.substmts:
                if has_mandatory(fd, is_config, l):
                    return True
    return False


def has_stats(stmt):
    for s in stmt.substmts:
        if (s.keyword == 'config' and s.arg == 'false'):
            return True;
        if len(stmt.substmts) == 0:
            return False;
        if has_stats(s):
            return True

def is_hidden(fd, stmt):
    for s in stmt.substmts:
        if (isinstance(s.keyword, tuple)):
            (a,b) = s.keyword
            if (a == 'tailf-common') and (b == 'hidden') and \
                    (s.arg == 'full'):
                return True
    return False


def nonl(s):
    return s.replace('\n', ' ')


def is_stats_leafref(s):
    if (hasattr(s, 'i_leafref_ptr') and
        s.i_leafref_ptr is not None):
        (ref, _pos) = s.i_leafref_ptr
        if (ref.i_config):
            ss = 'True'
        else:
            ss = 'False'
        if (ref.i_config == False):
            return True
    return False

def is_leafref(s):
    if (s.arg == 'leafref'):
        return True
    if (hasattr(s, 'i_leafref_ptr') and
        s.i_leafref_ptr is not None):
        return True
    return False

def patch_augment_in_augment(ctx, stmt, where, rpcprefix="", rpcname=""):
    if (stmt.keyword != 'augment'):
        return stmt
    np = stmt.arg;
    p = ctx.ncs_prefix
    if (where == 'template'):
        h = '/%s:devices/%s:template/%s:config' % (p,p,p)
    elif (where == 'device'):
        h = '/%s:devices/%s:device/%s:config' % (p,p,p)
    elif (where == 'live'):
        h = '/%s:devices/%s:device/%s:live-status' % (p,p,p)
    elif (where == 'notification'):
        h = '/%s:devices/%s:device/%s:netconf-notifications/%s:received-notifications/%s:notification/%s:data' % (p,p,p,p,p,p)
    elif (where == 'rpc'):
        h = '/%s:devices/%s:device/%s:rpc/%srpc-%s' % (p,p,p,rpcprefix,rpcname)
    else:
        fatal('bad where %s\n' % where)
    stmt.arg = h +  np
    return stmt

def emit_stmt(tfp, module, inaug, useraug, ctx, stmt, fd, indent,
              indentstep, inside_action, inside_rpc, inside_grouping,
              where, depth=0,add_device_type=False, lref_to_string=False):
    ncsp = ctx.ncs_prefix
    keyword = stmt.keyword
    if (keyword == 'notification'):
        return;

    ## drop all restrictions in the template/live tree
    if ( (where == 'template') or (where == 'live')):
        if (keyword in [ 'mandatory', 'min-elements', 'max-elements'
                         'must', 'default']):
            fd.write(indent + '// NCS drop  (' + keyword + ') statement\n')
            return;

    ## drop all tailf and smiv2 annotations except tailf:code-name in templates
    ## (yanger will give an error if tailf:code-name is different
    ##  for different occurrences of a given node name)
    if (where == 'template'):
        if (util.is_prefixed(keyword) and
            ((keyword[0] == tailf and keyword[1] != 'code-name') or
             keyword[0] == smiv2)):
            return;

    ## drop all ordered-by in templates
    if (where == 'template'):
        if (keyword == 'ordered-by'):
            fd.write(indent + '// NCS drop  (' + keyword + ') statement\n')
            return;

    ## Drop all deviation statements since we (yet) have no way handling
    ## this.

    if (keyword == 'deviation'):
        fd.write(indent + '// Deviation statement dropped, to make this work\n')
        fd.write(indent + '// ensure the base type covers all variations.\n')
        return;

    # leafreafs into stats data must be patched
    if ((inside_action or inside_rpc) and is_stats_leafref(stmt)):
        fd.write(indent + '// NCS patched type of leafref to stats\n')
        fd.write(indent + stmt.keyword + ' ' + stmt.arg + ' { type string; }\n')
        return

    if ((where == 'notification') and is_leafref(stmt)):
        fd.write(indent + '// NCS patched type leafref -> string\n')
        fd.write(indent + stmt.keyword + ' ' + stmt.arg + ' { type string; }\n')
        return


    if (is_hidden(fd, stmt)):
        fd.write(indent + '// NCS Ignoring hidden statement here\n')
        return
    if (keyword == 'grouping'):
        inside_grouping = True
        if (not stmt.arg in top_groupings):
            inner_groupings.append(stmt.arg)

    if (keyword == 'grouping' and where == 'toplevel'):
        if not hasattr(stmt, 'orig_arg'):
            stmt.orig_arg = stmt.arg

        if ( not ctx.opts.ncs_skip_config ):
            emit_stmt(tfp, module, inaug, useraug, ctx, stmt, fd, indent,
                      indentstep, inside_action, inside_rpc,
                      True, 'device', depth)
        if ( not ctx.opts.ncs_skip_statistics ):
            if (stmt.arg in top_groupings):
                stmt.arg = 'live_ncs_' + stmt.orig_arg
            emit_stmt(tfp, module, inaug, useraug, ctx, stmt, fd, indent,
                      indentstep, inside_action, inside_rpc,
                      True, 'live', depth, False, True)
            stmt.arg = stmt.orig_arg
        if ( not ctx.opts.ncs_skip_template ):
            if (stmt.arg in top_groupings):
                stmt.arg = 'template_ncs_' + stmt.orig_arg
            emit_stmt(tfp, module, inaug, useraug, ctx, stmt, fd, indent,
                      indentstep, inside_action, inside_rpc,
                      True, 'template', depth, False, True)

        return


    # Drop all stats statements unless in the live tree
    if is_stats(stmt) and (where not in ['live']):
        # we must not drop RowStatus entirely

        x = stmt.search_one('type')
        if (keyword == 'leaf'
            and x is not None
            and hasattr(x, 'i_typedef')
            and x.i_typedef is not None
            and x.i_typedef.i_module.i_modulename == 'SNMPv2-TC'
            and x.i_typedef.arg == 'RowStatus'
            and hasattr(stmt, 'i_smi_oid')):
            fd.write(indent+
                     "tailf:snmp-row-status-column %s;\n" %\
                     stmt.i_smi_oid[-1])
        else:
            fd.write(indent + '// NCS Ignoring stats ' +
                     util.keyword_to_str(stmt.raw_keyword) + ' block ' +
                     'called ' + nonl(stmt.arg) + ' here\n')
        return;

    # Possibly rewrite the uses arg
    if (keyword == 'uses'):
        parts = stmt.arg.split(':')

        if (((where == 'live') or
             (not ctx.opts.ncs_skip_statistics and
              (where == 'notification' or inside_rpc == True))) and
            ((len(parts) == 2) or (stmt.arg in top_groupings))):
            if (len(parts) == 1):
                starg = 'live_ncs_' + stmt.arg
            if (len(parts) == 2):
                starg = parts[0] + ':' + 'live_ncs_' + parts[1]
        elif (((where == 'live') or
             (not ctx.opts.ncs_skip_statistics and
              (where == 'notification' or inside_rpc == True))) and
              (not stmt.arg in inner_groupings) and
              (len(parts) == 1)):
            ## uses from a submodule
            starg = 'live_ncs_' + stmt.arg
        elif ((where == 'template') and (not stmt.arg in inner_groupings)
               and (len(parts) == 1)):
            ## uses from a submodule
            starg = 'template_ncs_' + stmt.arg

        elif ((where == 'template') and
              ((len(parts) == 2) or (stmt.arg in top_groupings))):
            if (len(parts) == 1):
                starg = 'template_ncs_' + stmt.arg
            if (len(parts) == 2):
                starg = parts[0] + ':' + 'template_ncs_' + parts[1]
        else:
            starg = stmt.arg

        fd.write(indent + 'uses ' + starg)
        if (len(stmt.substmts) == 0):
            fd.write(';\n')
        else:
            fd.write(' {\n')
            for sub in stmt.substmts:
                emit_stmt(tfp, module, inaug, useraug, ctx, sub, fd,
                          indent + indentstep,
                          indentstep, inside_action, inside_rpc,
                          inside_grouping, where, depth+1)
            fd.write(indent + '}\n')
        return

    if ((keyword == 'config') and (where == 'live')):
        return;

    # drop all mandatory statements in the notification containers
    # filters may make us not getting everything but we must still
    # be able to store it in CDB
    if ((where == 'notification') and
        (keyword in ['mandatory', 'min-elements'])):
        return
    if (keyword == (tailf, 'action')):
        inside_action = True
    if (keyword == 'config' and stmt.arg == 'true'):
        # Drop all explicit config true statements,
        fd.write(indent + '// NCS explicit config true stmt\n');
        return

    if ((keyword == 'type') and
        (where == 'notification') and
        (stmt.arg == 'instance-identifier')):
        fd.write(indent + '// NCS rewrite instance-identifier -> string\n')
        fd.write(indent + '// inside notification structure since device\n')
        fd.write(indent + '// may send pointers to models we do\'nt have\n')
        fd.write(indent + 'type string;\n')
        return;


    # It's not possible to have the config container above as a
    # presence statement, consider different devices.

    if inaug == True and depth == 1 and keyword == 'min-elements' and \
            stmt.parent.keyword == 'list':
        # we cannot have any length constraints on this list
        fd.write(indent+indentstep+
                 '// Dropping min-elements length restriction\n')
        return


    if inaug == True and depth == 1 and keyword == 'mandatory ' and \
            (stmt.parent.keyword == 'leaf' or \
                 stmt.parent.keyword == 'leaf-list'):
        fd.write(indent+indentstep+
                 '// Dropping mandatory restriction\n')
        return
    # Drop some tailf extensions including callpoint

    if ((util.is_prefixed(keyword) and keyword[0] == tailf) and
        (inside_action == False) and
        (inside_rpc == False) and
        (not keep_tailf_annotation(where, ctx, keyword[1]))):
        fd.write(indent + '// NCS drop tailf:' + keyword[1] +'\n')
        return;
    if (keyword == 'if-feature'):
        fd.write(indent + '// NCS drop if-feature statement\n')
        return;

    if ((keyword in ['must',
                     'when',
                     (tailf, 'display-when'),
                     (tailf, 'cli-diff-dependency'),
                     (tailf, 'unique-selector')]) and
        (where in ['live', 'template'])):
        fd.write(indent + '// NCS drop path statement here\n')
        return;

    if keyword == 'rpc':
        fd.write(indent + 'augment /%s:devices/%s:device/%s:rpc {\n' %
                 (ncsp, ncsp, ncsp))
        indent += indentstep
        fd.write(indent + 'container rpc-' + stmt.arg + ' {\n')
        indent += indentstep
        fd.write(indent + tfp + ':action ' + stmt.arg + ' {\n')
        fd.write(indent + '  ' + tfp + ':actionpoint ncsproxy {\n' +
                 indent +'        ' + tfp + ':internal;\n' + indent + '  }\n')
        for s in stmt.substmts:
            emit_stmt(tfp, module, inaug, useraug, ctx, s, fd,
                      indent + indentstep,
                      indentstep, inside_action, True, inside_grouping,
                      where, depth+1)
        fd.write(indent + '}\n   }\n   }\n')
    elif (inside_action == True) and ((keyword == (tailf, 'exec')) or
                                    (keyword == (tailf, 'actionpoint'))):
        fd.write(indent + tfp + ':actionpoint ncsproxy { ' + tfp +
                 ':internal; }\n')
    elif (inside_rpc == True) and  ((keyword == (tailf, 'exec')) or
                                    (keyword == (tailf, 'actionpoint'))):
        # actionpoint already added above
        return

    elif (keyword == (tailf, 'non-strict-leafref') and
          is_stats_leafref(stmt.parent)):
        # drop the stmt if it refers to non-config
        fd.write(indent +
                 '// NCS drop non-strict-leafref to stats\n')
        return
    else:
        if (keyword == (tailf, 'display-when') and depth == 1):
            # WE must drop user defined display-when at the top
            # level because we add our own display-when here
            fd.write(indent + indentstep +
                     '// NCS drop display-when here\n')
            return

        # drop must statement w/o dependencies, or if we cannot derive
        # an automatic dependency
        if (keyword in ['must', 'when']
            and stmt.search_one((tailf, 'dependency')) is None):
            auto_dep = tailf_plugin.xpath_get_tagpath(stmt.arg)
            if auto_dep is None:
                fd.write(indent +
                         '// NCS drop %s w/o dependency here\n' % keyword)
                return

        fd.write(indent + util.keyword_to_str(stmt.raw_keyword))
        if stmt.arg != None:

            if where == 'template' and keyword in ['list', 'leaf-list']:
                fd.write (' ' + stmt.arg+' {\n')
                fd.write(indent + indentstep +
                         'ordered-by "user";  ' +
                         '// NCS patched to keep predictable order\n')
                for ss in stmt.substmts:
                    emit_stmt(tfp, module, inaug, useraug, ctx, ss, fd,
                              indent + indentstep,
                              indentstep, inside_action, inside_rpc,
                              inside_grouping, where, depth+1)
                fd.write(indent + '}\n')
                return

            if where == 'template' and keyword == 'type':
                if stmt.arg == 'empty':
                    fd.write (' empty;\n')
                else:
                    fd.write (' string;   // NCS patched all types to string\n')
                return

            if ((inside_action or inside_rpc or (where == 'notification')) and
                (keyword == 'type') and is_leafref(stmt)):
                fd.write (' string;   // leafref patched to string\n')
                return
            if keyword in ['must', 'when',
                           (tailf, 'display-when'), (tailf, 'unique-selector')]:
                emit_arg(stmt.arg, fd, indent, indentstep)
                fd.write(' {\n')
                level=3;
                if (where == 'notification'):
                    level=6
                fd.write(indent + indentstep +
                         tfp + ':xpath-root \"%s\";\n' % level)
                for ss in stmt.substmts:
                    emit_stmt(tfp, module, inaug, useraug, ctx, ss, fd,
                              indent + indentstep,
                              indentstep, inside_action, inside_rpc,
                              inside_grouping, where, depth+1)
                fd.write(indent + '}\n')
                return;
            elif keyword in grammar.stmt_map:
                (arg_type, _subspec) = grammar.stmt_map[keyword]
                if arg_type in ['identifier', 'identifier-ref', 'boolean']:
                    fd.write(' ' + stmt.arg)
                elif ((arg_type in ['path-arg']) or
                      (keyword == (tailf, 'dependency')) or
                      (keyword == (tailf, 'cli-diff-dependency'))):
                    if ((stmt.arg[0] == '/') and (where == 'toplevel')):
                        # paths inside groupings cannot be made into
                        # relative paths,
                        if is_stats_leafref(stmt.parent.parent):
                            arg = '/%s:devices/%s:device/%s:live-status%s' % \
                                (ncsp, ncsp, ncsp, stmt.arg)
                        else:
                            arg = '/%s:devices/%s:device/%s:config%s' % \
                                (ncsp, ncsp, ncsp, stmt.arg)
                        emit_arg(arg, fd, indent, indentstep)
                        fd.write(' {\n')
                        level=3;
                        if (where == 'notification'):
                            level=6
                        fd.write(indent + indentstep +
                                 tfp + ':xpath-root \"%s\";\n' % level)
                        for ss in stmt.substmts:
                            emit_stmt(tfp, module, inaug, useraug, ctx, ss, fd,
                                      indent + indentstep,
                                      indentstep, inside_action, inside_rpc,
                                      inside_grouping, where, depth+1)
                        fd.write(indent + '}\n')
                        return
                    elif ((stmt.arg[0] == '/') and
                          ((useraug == True) or (inside_grouping == True)) and
                          (where in ['device', 'template','live','toplevel'])):

                        if (where == 'toplevel'):
                            if is_stats_leafref(stmt.parent.parent):
                                xtra ='/%s:devices/%s:device/%s:live-status' % \
                                    (ncsp, ncsp, ncsp)
                            else:
                                xtra = '/%s:devices/%s:device/%s:config' % \
                                    (ncsp, ncsp, ncsp)
                        if (where == 'device'):
                            xtra = '/%s:devices/%s:device/%s:config' % \
                                (ncsp, ncsp, ncsp)
                        elif (where == 'template'):
                            xtra = '/%s:devices/%s:template/%s:config' % \
                                (ncsp, ncsp, ncsp)
                        elif (where == 'live'):
                            xtra = '/%s:devices/%s:device/%s:live-status' % \
                                (ncsp, ncsp, ncsp)
                        arg = xtra + stmt.arg

                        emit_arg(arg, fd, indent, indentstep)
                        fd.write(' {\n')
                        level=3;
                        if (where == 'notification'):
                            level=6
                        fd.write(indent + indentstep +
                                 tfp + ':xpath-root \"%s\";\n' % level)
                        for ss in stmt.substmts:
                            emit_stmt(tfp, module, inaug, useraug, ctx, ss, fd,
                                      indent + indentstep,
                                      indentstep, inside_action, inside_rpc,
                                      inside_grouping, where, depth+1)
                        fd.write(indent + '}\n')
                        return
                    elif ((stmt.arg[0] == '/')):
                        # Rewrite absolute path to relative
                        # Note: everything but cli-diff-dependency
                        # starts out one step "below" its statement,
                        # hence the "offset" calculation
                        if (stmt.keyword == (tailf, 'cli-diff-dependency')):
                            off = 0
                        else:
                            off = 1
                        arg = "../" * (depth-off) + stmt.arg[1:]
                    else:
                        # relative path
                        arg = stmt.arg
                    emit_arg(arg, fd, indent, indentstep)
                else:
                    emit_arg(stmt.arg, fd, indent, indentstep)
            else:
                emit_arg(stmt.arg, fd, indent, indentstep)

        if len(stmt.substmts) == 0:
            if add_device_type:
                fd.write('  {\n' + indent + indentstep +
                         'tailf:ncs-device-type %s;\n' % \
                             ctx.opts.ncs_device_type +
                         indent + '}\n')
            else:
                fd.write(';\n')
        else:
            fd.write(' {\n')
            if add_device_type:
                fd.write(indent + indentstep +
                         'tailf:ncs-device-type %s;\n' % \
                             ctx.opts.ncs_device_type)
            if (inaug == True) and (depth == 0):
                if ((keyword == 'leaf') and
                    (has_mandatory(fd, (where == 'device') or
                                       (where == 'template'), stmt))):
                    tmp = []
                    for sub in stmt.substmts:
                        if sub.keyword != 'mandatory':
                            tmp = tmp + [sub]
                    stmt.substmts = tmp
                if ((keyword == 'container') and
                    (stmt.search_one('presence') == None) and
                    (has_mandatory(fd, (where == 'device') or
                                   (where == 'template'), stmt))):
                    fd.write(
                        indent+indentstep+
                        "presence \"In NCS this element is now optional\";\n")
                if (where == 'device'):
                    if (useraug == True):
                        True
                    elif (stmt.keyword == 'list'):
                        fd.write(
                            indent+indentstep+tfp+
                            ":display-when \"not(../%s:module) or ../%s:module[%s:name='%s']\";\n" % (ncsp, ncsp, ncsp, module.display_when_mod))
                    else:
                        fd.write(
                            indent+indentstep+tfp+
                            ":display-when \"not(../../%s:module) or ../../%s:module[%s:name='%s']\";\n" % (ncsp, ncsp, ncsp, module.display_when_mod))

            substmts = stmt.substmts
            for s in substmts:
                if ((keyword == 'choice' ) or (keyword == 'case')):
                    depth2=depth
                else:
                    depth2=depth+1
                emit_stmt(tfp, module, inaug, useraug, ctx, s, fd,
                          indent + indentstep,
                          indentstep, inside_action, inside_rpc,
                          inside_grouping, where, depth2)
            fd.write(indent + '}\n')



def emit_arg(arg, fd, indent, indentstep):
    """Heuristically pretty print the argument string"""
    # current alg. always print a double quoted string
    arg = arg.replace('\\', r'\\')
    arg = arg.replace('"', r'\"')
    arg = arg.replace('\t', r'\t')
    lines = arg.splitlines()
    if len(lines) <= 1:
        fd.write(' "' + arg + '"')
    else:
        fd.write('\n')
        fd.write(indent + indentstep + '"' + lines[0] + '\n')
        for line in lines[1:-1]:
            fd.write(indent + indentstep + ' ' + line + '\n')
        fd.write(indent + indentstep + ' ' + lines[-1] + '"')


def fatal(s, exitCode=1):
    raise error.EmitError(s, exitCode)
