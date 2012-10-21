



#import os
#import sys
#local_dir = os.path.split(__file__)[0]
#sys.path.append( os.path.join(local_dir,'../..') )


from django.core.management import setup_environ
import settings
setup_environ(settings)

from pdtweb.models import MonitoredFile, Profile




_filenames = """
/home/michael/hw_to_come/morphforge/doc/conf.py
/home/michael/hw_to_come/morphforge/devscripts/MF_create_example_docs.py
/home/michael/hw_to_come/morphforge/src/morphforge/units/util.py
/home/michael/hw_to_come/morphforge/src/morphforge/units/wrappers.py
/home/michael/hw_to_come/morphforge/src/morphforge/units/common_neuroscience_defs.py
/home/michael/hw_to_come/morphforge/src/morphforge/units/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/op_fixeddt_fixeddt.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/op_variabledt_scalar.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/op_fixeddt_quantity.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/op_piecewise_scalar.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/op_fixeddt_scalar.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/variable_dt_rebasing.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/operators/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/traceobjpluginctrl.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/eventset.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tags/tagselectorstringparser.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tags/tagselector.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tags/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/io/traceio.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/io/tocsv.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/io/fromcsv.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/io/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/trace.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/tracepiecewise.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/tracevariabledt.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/tracepointbased.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/tracefixeddt.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/tracetypes/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/generation/generator_parser.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/generation/gen_parser_yacc.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/generation/gen_parser_lexer.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/generation/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_splicing.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_integrate.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/MMtrace_conversion.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_fft.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_filters.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_math.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_clone.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/trace_methods_std_conversions.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/methods/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/traces/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/util/morphlocator.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/util/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/conventions/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/errors/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/exporter/morphologyexporter.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/exporter/export_tree_swc.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/exporter/export_array_swc.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/exporter/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/visitor/visitorfactory.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/visitor/morphologyoperators.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/visitor/visitorbaseclasses.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/visitor/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/comparison/comparearrays.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/comparison/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/mesh/writer_ply.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/mesh/util.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/mesh/builder_rings.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/mesh/mesh.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/mesh/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/ui/morphmaths.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/ui/matplotlibviewer.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/ui/mayavirenderer.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/ui/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/core/morphologyconsistency.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/core/tree.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/core/base.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/core/array.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/core/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/conversion/region_to_int_bimap.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/conversion/internal_representation_converter.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/conversion/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/builders/morphologybuilder.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/builders/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/importer/import_array_swc.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/importer/morphologyimporter.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/importer/import_tree_dictionary.py
/home/michael/hw_to_come/morphforge/src/morphforge/morphology/importer/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/constants/ions.py
/home/michael/hw_to_come/morphforge/src/morphforge/constants/standardtags.py
/home/michael/hw_to_come/morphforge/src/morphforge/constants/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/summaries_new/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/tagviewer/plotspecs.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/tagviewer/tagviewer.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/tagviewer/post_ax_functors.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/tagviewer/linkage/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/tagviewer/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulationanalysis/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/management/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/objectnumberer.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/monkey_patching.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mfrandom.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mgrs/settingsmgr.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mgrs/logmgr.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mgrs/rcmgr.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mgrs/locmgr.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mgrs/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/plugindict.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/mockcontrol.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/misc.py
/home/michael/hw_to_come/morphforge/src/morphforge/core/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/stimulation/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/util/celllocator.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/util/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/simbundle.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/postsimulation/postsimulationaction.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/postsimulation/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/builders/builders.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/builders/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/simulationmetadatabundle/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/synaptictriggers/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/channeltargetters.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/passiveproperties.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/parsetab.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/channel.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/channelapplicators.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/cellbiophysics.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/biophysics/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/base_classes.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/networks/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/result/simulationresult.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/result/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/recordable.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/celllocation.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/simulation.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/simulationenvironment.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/cell.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/core/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/segmentation/cellsegmenter.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/segmentation/segment.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/segmentation/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/base/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/modfilewriterbase.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/hocmodutils.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/hocbuilder.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/modfilesectioned.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/hocbuilder_cell.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/hocmodbuilders/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/simulationdatacontainers/mhocfile.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/simulationdatacontainers/mmodfileset.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/simulationdatacontainers/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/synaptictriggers/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/biophysics/modfile.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/biophysics/mm_neuron.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/biophysics/modfilecompiler.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/biophysics/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/networks/__init__.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/core/neuronsimulationsettings.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/core/neuronsimulation.py
/home/michael/hw_to_come/morphforge/src/morphforge/simulation/neuron/core/neuronsimulationenvironment.py
"""




filenames = [f.strip() for f in _filenames.split('\n') if f.strip()]















        
def update_file(filename):
    try:
        obj = MonitoredFile.objects.get(full_filename=filename)
    except MonitoredFile.DoesNotExist:
        obj = MonitoredFile.create(full_filename=filename)
        obj.save()
    obj.ensure_up_to_date()
        
        
        
        

        
        

def main():
    
    # Scan for changes in all tracked files:
    for filename in filenames:
        update_file(filename)
    
    
    #initialise_notify()
    #initialise_notify()
    
    
    
if __name__=='__main__':
    main()
    
    
    



import os
from pyinotify import WatchManager, Notifier, ThreadedNotifier, EventsCodes, ProcessEvent

class PTmp(ProcessEvent):
    def process_IN_CREATE(self, event):
        print "Create: %s" %  os.path.join(event.path, event.name)

    def process_IN_DELETE(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)
    
    def process_IN_MODIFY(self, event):
        print "Remove: %s" %  os.path.join(event.path, event.name)
        
        
