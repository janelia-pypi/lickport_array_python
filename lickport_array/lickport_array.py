import pathlib
import numpy
import matplotlib.pyplot as plt

try:
    from pkg_resources import get_distribution, DistributionNotFound
    import os
    _dist = get_distribution('lickport_array')
    # Normalize case for Windows systems
    dist_loc = os.path.normcase(_dist.location)
    here = os.path.normcase(__file__)
    if not here.startswith(os.path.join(dist_loc, 'lickport_array')):
        # not installed, but there is another version that *is*
        raise DistributionNotFound
except (ImportError,DistributionNotFound):
    __version__ = None
else:
    __version__ = _dist.version


class LickportArray():
    '''
    '''

    def __init__(self,*args,**kwargs):
        base_path = pathlib.Path(__file__).resolve().parent
        data_path = base_path.joinpath('data')
        self.runs = {}
        for data_file in data_path.iterdir():
            data_file_stem = data_file.stem
            run = numpy.genfromtxt(data_file,delimiter=',')
            run = run[:,[3,4]]
            self.runs[data_file_stem] = run
        self.runs = {k: v for k, v in sorted(self.runs.items(), key=lambda item: item[0])}
        self.output_path = pathlib.Path.home() / 'output'
        self.output_path.mkdir(parents=True,exist_ok=True)

    def plot(self):
        subplot_index = 1
        for name, run in self.runs.items():
            plt.subplot(4,2,subplot_index)
            subplot_index += 1
            plt.plot(run[:,0],run[:,1])
            plt.ylim([-0.2,0.2])
            plt.title(name)
        plt.tight_layout()
        plot_path = self.output_path / 'plots.png'
        plt.savefig(plot_path)
                
# -----------------------------------------------------------------------------------------
if __name__ == '__main__':

    la = LickportArray()
    la.plot()
