import time
from threading import Timer

from modular_client import ModularClient

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


DEBUG = False

class LickportArray():
    '''
    '''
    _CHECK_DATA_PERIOD = 1.0
    def __init__(self,*args,**kwargs):
        if 'debug' in kwargs:
            self.debug = kwargs['debug']
        else:
            kwargs.update({'debug': DEBUG})
            self.debug = DEBUG
        self.dev = ModularClient()
        self.dev.set_time(int(time.time()))

    def start_check_data_timer(self):
        self._check_data_timer = Timer(self._CHECK_DATA_PERIOD,self._check_data)
        self._check_data_timer.start()
        print("Check data timer started")

    def _check_data(self):
        self._check_data_timer.start()
        data = self.dev.get_and_clear_lick_data()
        if len(data) > 0:
            print(data)

def main(args=None):
    debug = False
    dev = LickportArray(debug=debug)
    dev.start_check_data_timer()

# -----------------------------------------------------------------------------------------
if __name__ == '__main__':
    main()
