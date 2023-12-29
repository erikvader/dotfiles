from ..types import SiteName, Scraper
from typing import Optional
from .rarbg import rarbgScraper
from .ph import PHScraper
from .xvideos import XVideosScraper
from .trettontrettiosju import trettontrettiosjuScraper
from .tnaflix import TnaFlixScraper
from .spankbang import SpankbangScraper
from .xnxx import XnxxScraper
from .theyarehuge import theyAreHugeScraper
from .myhentaigallery import myhentaigalleryScraper
from .xhamster import XHamsterScraper
from .iwara import IwaraScraper
from .yt import YTScraper
from .ehentai import ehentaiScraper
from .nhentai import nhentaiScraper
from .hanime import hanimeScraper


def get_scraper(hostname: SiteName) -> Optional[Scraper]:
    return {
        "rarbg": rarbgScraper,
        "pornhub": PHScraper,
        "xvideos": XVideosScraper,
        "1337x": trettontrettiosjuScraper,
        "tnaflix": TnaFlixScraper,
        "spankbang": SpankbangScraper,
        "xnxx": XnxxScraper,
        "theyarehuge": theyAreHugeScraper,
        "myhentaigallery": myhentaigalleryScraper,
        "xhamster": XHamsterScraper,
        "iwara": IwaraScraper,
        "youtube": YTScraper,
        "e-hentai": ehentaiScraper,
        "nhentai": nhentaiScraper,
        "hanime": hanimeScraper,
    }.get(hostname)
