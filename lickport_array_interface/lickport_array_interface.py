import time
from threading import Timer
import atexit
import pathlib
from datetime import datetime
import csv

from modular_client import ModularClient

try:
    from pkg_resources import get_distribution, DistributionNotFound
    import os
    _dist = get_distribution('lickport_array_interface_interface')
    # Normalize case for Windows systems
    dist_loc = os.path.normcase(_dist.location)
    here = os.path.normcase(__file__)
    if not here.startswith(os.path.join(dist_loc, 'lickport_array_interface_interface')):
        # not installed, but there is another version that *is*
        raise DistributionNotFound
except (ImportError,DistributionNotFound):
    __version__ = None
else:
    __version__ = _dist.version


DEBUG = False

class LickportArrayInterface():
    '''
    '''
    _DATA_PERIOD = 5.0
    def __init__(self,*args,**kwargs):
        if 'debug' in kwargs:
            self.debug = kwargs['debug']
        else:
            kwargs.update({'debug': DEBUG})
            self.debug = DEBUG
        atexit.register(self._exit)
        self.controller = ModularClient(*args,**kwargs)
        self.controller.set_time(int(time.time()))
        self.controller.calibrate_lick_sensor()
        self.data = []
        self._data_period = self._DATA_PERIOD
        self._base_path = pathlib.Path('~/lickport_array_data').expanduser()
        self._data_filename = 'data.csv'
        self._acquiring_data = False
        self._saving_data = False
        self._data_fieldnames = ['',
                                 '',
                                 '',
                                 '']

    def start_acquiring_data(self,data_period=None):
        if data_period:
            self._data_period = data_period
        else:
            self._data_period = self._DATA_PERIOD
        self._start_data_timer()
        self._acquiring_data = True

    def stop_acquiring_data(self):
        self._data_timer.cancel()
        self._acquiring_data = False

    def start_saving_data(self):
        data_directory_path = self._base_path / self._get_date_time_str()
        data_directory_path.mkdir(parents=True,exist_ok=True)
        data_file_path = data_directory_path / self._data_filename
        self._data_file = open(data_file_path,'w')
        self._data_writer = csv.DictWriter(self._data_file,fieldnames=self._data_fieldnames)
        self._data_writer.writeheader()
        print('Created: {0}'.format(data_file_path))
        self._saving_data = True
        if not self._acquiring_data:
            self.start_acquiring_data()

    def stop_saving_data(self):
        self._saving_data = False
        self._data_file.close()

    def _handle_data(self):
        data = self.controller.get_and_clear_lick_data()
        if len(data) > 0:
            for datum in data:
                print(datum)
            self.data.extend(data)
            with open(self._data_file_path, 'a') as outfile:
                json.dump(self.data, outfile)
        self._start_data_timer()

    def _start_data_timer(self):
        self._data_timer = Timer(self._data_period,self._handle_data)
        self._data_timer.start()

    def _exit(self):
        try:
            self.stop_saving_data()
        except AttributeError:
            pass

    def _get_date_time_str(self,timestamp=None):
        if timestamp is None:
            d = datetime.fromtimestamp(time.time())
        elif timestamp == 0:
            date_time_str = 'NULL'
            return date_time_str
        else:
            d = datetime.fromtimestamp(timestamp)
        date_time_str = d.strftime('%Y-%m-%d-%H-%M-%S')
        return date_time_str



def main(args=None):
    lai = LickportArrayInterface()
    lai.start_saving_data()

# -----------------------------------------------------------------------------------------
if __name__ == '__main__':
    main()
