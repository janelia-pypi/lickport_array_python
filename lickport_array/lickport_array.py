import pathlib
import numpy
import matplotlib.pyplot as plt
from scipy.fft import fft, fftfreq
from scipy import signal

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
        self.plot_time()
        self.plot_trans()
        # self.plot_raw_freq()
        self.plot_filt_freq()

    def plot_time(self):
        subplot_index = 1
        for name, run in self.runs.items():
            plt.subplot(4,2,subplot_index)
            subplot_index += 1
            plt.plot(run[:,0],run[:,1])
            plt.ylim([-0.05,0.05])
            plt.title(name)
        plt.tight_layout()
        plt.suptitle('Time Plots')
        plot_path = self.output_path / 'time_plots.png'
        plt.savefig(plot_path)
        plt.close()
                
    def plot_trans(self):
        subplot_index = 1
        for name, run in self.runs.items():
            plt.subplot(4,2,subplot_index)
            subplot_index += 1
            median = numpy.median(run[:,1])
            trans = numpy.abs(run[:,1] - median)
            plt.plot(run[:,0],trans)
            plt.ylim([-0.001,0.025])
            plt.title(name)
        plt.tight_layout()
        plt.suptitle('Trans Plots')
        plot_path = self.output_path / 'trans_plots.png'
        plt.savefig(plot_path)
        plt.close()
                
    def plot_raw_freq(self):
        subplot_index = 1
        for name, run in self.runs.items():
            plt.subplot(4,2,subplot_index)
            subplot_index += 1
            sample_count = run.shape[0]
            sample_period = run[1,0] - run[0,0]
            yf = fft(run[:,1])
            xf = fftfreq(sample_count,sample_period)
            plt.plot(xf,numpy.abs(yf))
            plt.title(name)
        plt.tight_layout()
        plt.suptitle('Freq Plots Raw')
        plot_path = self.output_path / 'raw_freq_plots.png'
        plt.savefig(plot_path)
        plt.close()
                
    def plot_filt_freq(self):
        b,a = signal.butter(4,0.5,btype='highpass',analog=True)
        subplot_index = 1
        for name, run in self.runs.items():
            plt.subplot(4,2,subplot_index)
            subplot_index += 1
            y = signal.filtfilt(b,a,run[:,1])
            plt.plot(run[:,0],y)
            plt.ylim([-0.3,0.3])
            plt.title(name)
        plt.tight_layout()
        plt.suptitle('Freq Plots Filt')
        plot_path = self.output_path / 'filt_freq_plots.png'
        plt.savefig(plot_path)
        plt.close()
                
# -----------------------------------------------------------------------------------------
if __name__ == '__main__':

    la = LickportArray()
    la.plot()
