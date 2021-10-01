import pathlib
import flatten_json

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
    def __init__(self,*args,**kwargs):
        if 'debug' in kwargs:
            self.debug = kwargs['debug']
        else:
            kwargs.update({'debug': DEBUG})
            self.debug = DEBUG
        self.dev = ModularClient()
        self.dev.set_time(int(time.time()))

# -----------------------------------------------------------------------------------------
if __name__ == '__main__':

    debug = False
    dev = LickportArray(debug=debug)
