#!/usr/bin/env python3

import html
import html.parser
import sys, os
import asyncio
import aiohttp
import cchardet

from collections import OrderedDict

import heapq
import time
import logging
import re

logging.basicConfig(
    filename=os.path.abspath('./hello.log'), 
    filemode="w", 
    level=logging.DEBUG, 
    datefmt="%H:%M:%S",
    format='[%(asctime)s, %(msecs)d %(name)s %(levelname)s]: %(message)s'
    )
logger = logging.getLogger(__name__)
logger.debug("BEGIN DEBUG")

time_tolerance = 300

class CCError(Exception):
    pass
class InvalidParameter(CCError):
    pass

book_dict: OrderedDict = OrderedDict()
book_dir: str = os.path.join(os.path.abspath('.'), "books")
if not os.path.exists(book_dir):
    os.makedirs(book_dir)
else:
    if not os.path.isdir(book_dir):
        raise CCError("Book directory invalidated")


class NovelQueue():
    __priorityQueue: list # id
    last: int
    last_time: float
    dir_name: str

    def __init__(self, dir: str):
        self.__priorityQueue = list()
        self.last = 0
        self.last_time = 0
        self.dir_name = dir

    def Empty(self):
        return self.__priorityQueue.__len__() == 0

    def Top(self):
        assert not self.Empty()
        return self.__priorityQueue[0]

    def ExtractMin(self):
        assert not self.Empty()
        return heapq.heappop(self.__priorityQueue)

    def Insert(self, item, context):
        try:
            if not os.path.exists(self.dir_name):
                os.makedirs(self.dir_name)
            if not os.path.isdir(self.dir_name):
                logger.warning(f"path {self.dir_name} is not a directory")
                return
            file_name = os.path.join(self.dir_name, f"{item}.txt")
            if not file_name:
                return
            with open(file_name, "w") as _file:
                _file.write(context)
        except:
            return
        self.Insert_ID(item)

    def Insert_ID(self, item):
        heapq.heappush(self.__priorityQueue, item)

async def book_dict_merge_try():
    try:
        while True:
            await asyncio.sleep(60 * 6)
            logger.info("book dictionary merger begin work.")
            for _, q in book_dict:
                __check_context__(q)
    except:
        logger.info("Book dictionary merger exit.")

async def bbbbbbbb(cors):
    book_merger_task = asyncio.create_task(book_dict_merge_try())
    await asyncio.create_task(cors)
    book_merger_task.cancel()

def AppendContext(name: str, id: int, context: str):
    dir_name = os.path.join(book_dir, f"{name}")
    if not book_dict.__contains__(name):
        new_queue = NovelQueue(dir_name)
        book_dict[name] = new_queue
        new_queue.last = id
        new_queue.Insert(id, context)
        new_queue.ExtractMin()
        new_queue.last_time = time.time()
    else:
        queue: NovelQueue = book_dict[name]
        queue.Insert(id, context)
        __check_context__(queue)
    return

def __check_context__(queue: NovelQueue):
    if(queue.Empty()):
        return
    top_id = queue.Top()
    assert top_id != queue.last
    if top_id < queue.last:
        queue.Insert_ID(queue.last)
        queue.last = queue.ExtractMin()
        __check_context__(queue)
    elif top_id == queue.last + 1 or (time.time() - queue.last_time > time_tolerance):
        app_file = os.path.join(queue.dir_name, f"{queue.last}.txt")
        read_file = os.path.join(queue.dir_name, f"{top_id}.txt")
        with open(app_file, "a") as _afile:
            with open(read_file, "r") as _rfile:
                _afile.write(_rfile.read())
                queue.last_time = time.time()
                queue.last = queue.ExtractMin()
            os.remove(read_file)
        os.rename(app_file, read_file)
        __check_context__(queue)
    else:
        return

# [http://] host/book/chapter.html
html_url_regex_3: re.Pattern = re.compile(r"^(\ *http:\/\/)?([^/]*)?\/([^/]*)\/([^/]*)\.html$")
html_url_regex_2: re.Pattern = re.compile(r"^(\ *http:\/\/)?([^/]*)?\/([^/]*)\/$")

class htmlParser(html.parser.HTMLParser):
    HasContent: bool
    BookName: str
    Content: str
    __waited_list: list
    __waited_list_lock: asyncio.locks.Lock

    __new_waited_list: list
    __tag_level: int
    __is_html: bool
    __begin_content: bool
    __content_level: int
    __next_is_chapter: int
    __follow_has_book_name: int

    def __init__(self, waited_list: list, waited_list_lock: asyncio.locks.Lock, host: str):
        super(htmlParser, self).__init__()
        self.HasContent = False
        self.BookName = None
        self.Content = ""

        self.__waited_list = waited_list
        self.__waited_list_lock = waited_list_lock
        self.__host = host

        self.__tag_level = 0
        self.__is_html = False
        self.__begin_content = False
        self.__new_waited_list = []
        self.__content_level = 0
        self.__next_is_chapter = 0
        self.__follow_has_book_name = 0

    def handle_starttag(self, tag: str, attrs):
        if not self.__is_html:
            return
        self.__tag_level += 1
        __set_title = False
        if self.__content_level > 0:
            self.__content_level += 1
        if tag == "meta":
            for k, v in attrs:
                if __set_title:
                    if k == "content":
                        self.BookName = v
                if k == "property" and v == "og:title":
                    __set_title = True
                    continue

        if tag == "a":
            for k, v in attrs:
                if k == "href":
                    if self.__follow_has_book_name > 0:
                        self.__follow_has_book_name += 1
                        if self.__follow_has_book_name > 4:
                            self.__follow_has_book_name = 0
                        return
                    m_v = html_url_regex_2.match(v)
                    if m_v is not None:
                        self.__new_waited_list.append("http://" + self.__host + "/" + m_v.group(3) + "/")
                        return
                    m_v = html_url_regex_3.match(v)
                    if m_v is not None:
                        self.__new_waited_list.append("http://" + self.__host + "/" + m_v.group(3) + "/" + m_v.group(4) + ".html")
                    return

        if tag == "h1" and self.__next_is_chapter == 1:
            self.__next_is_chapter = 2

        if tag == "div":
            for k, v in attrs:
                if k == "id" and v == "content":
                    self.__begin_content = True
                    self.__content_level = 1
                    self.HasContent = True
                if k == "class" and v == "bookname":
                    self.__next_is_chapter = 1
                if k == "class" and v == "con_top":
                    self.__follow_has_book_name = 1
            return

    def __append_list(self):
        logger.debug(f"add url list: {self.__new_waited_list.__len__()}")
        self.__waited_list += self.__new_waited_list
        self.__new_waited_list = []

    def handle_endtag(self, tag: str):
        if not self.__is_html:
            return
        self.__tag_level -= 1
        if self.__content_level > 0:
            self.__content_level -= 1
        if self.__content_level == 0:
            self.__begin_content = False
        if self.__tag_level <= 2:
            self.__append_list()

    def handle_data(self, data: str):
        if not self.__is_html:
            return
        if self.__follow_has_book_name == 4:
            self.__follow_has_book_name = 0
            self.BookName = data.strip()
            return
        if self.__next_is_chapter == 2:
            self.__next_is_chapter = 0
            self.Content = data.strip() + "\n\n"
            return
        if not self.__begin_content:
            return
        if len(data) <= 4:
            return
        self.Content += data.strip()
        self.Content += "\n\n"

    def handle_comment(self, data: str):
        pass

    def handle_entityref(self, name: str):
        pass

    def handle_charref(self, name: str):
        pass

    def handle_decl(self, data: str):
        mm = re.match(r"^.*html.*$", data)
        self.HasContent = False
        self.BookName = None
        self.Content = ""
        self.__begin_content = False
        self.__new_waited_list = []
        self.__tag_level = 0
        self.__next_is_chapter = 0
        self.__follow_has_book_name = 0
        if mm is not None:
            self.__is_html = True
        else:
            self.__is_html = False


class CrawlerWhat():
    def __init__(self, host, port = 80, visited_savefile = "crawler_visited", waited_savefile = "crawler_waited", startLocation="/", client_num = 10):
        self.__runner_alter_cond: asyncio.locks.Condition = asyncio.locks.Condition()
        self.__host = host
        self.__port = port
        self.__start_location = startLocation
        self.__waited_queue: list = [self.__start_location]
        self.__lock_waited_queue = asyncio.locks.Lock()
        self.__client_list = HttpClientList(host, port, self.__runner_alter_cond, (self.__waited_queue, self.__lock_waited_queue), timeout = 100, num = client_num) # TODO, pass state to parser
        self.__save_visited = visited_savefile
        self.__save_waited  = waited_savefile
        self.__lock_visited_table = asyncio.locks.Lock()
        self.__visited_table = OrderedDict()
        self.__timer_task = None

    # debug ...
    async def __runner_cond_timer(self):
        try:
            while True:
                # logger.debug("runner condition notifier, beat ...")
                await asyncio.sleep(1)
                async with self.__runner_alter_cond:
                    self.__runner_alter_cond.notify()
        except:
            # logger.debug("runner condition notifier end")
            pass

    def readFromLocal(self):
        self.__save_visited = []
        self.__waited_queue = []
        with open(self.__save_visited, "r") as _file:
            while True:
                ss = _file.readline().strip()
                if ss is None:
                    break
                self.__visited_table[ss] = True
        with open(self.__waited_queue, "r") as _file:
            while True:
                ss = _file.readline().strip()
                if ss is None:
                    break
                self.__waited_queue += [ss]

    def saveToLocal(self):
        with open(self.__save_visited, "w") as _file:
            for k, _ in self.__visited_table:
                _file.writelines(k)
        with open(self.__waited_queue, "w") as _file:
            for ww in self.__waited_queue:
                _file.writelines(ww)

    async def __fetch_and_feed(self, client: aiohttp.client.ClientSession, url, parser: htmlParser, client_num: int):
        # logger.debug(f"client - {client_num}, try to get url <{url}>")
        resp: aiohttp.client.ClientResponse
        while True:
            async with client.get(url) as resp:
                if resp.status < 200 or resp.status >= 300:
                    logger.warning(f"server response - {resp.status}")
                    break
                try:
                    html_data = await resp.text()
                except UnicodeDecodeError as err:
                    logger.warning(err)
                    break
                # print(html_data)
                parser.feed(html_data)
                m_v = html_url_regex_3.match(url)
                if m_v is None or m_v.group(4) == "":
                    logger.warning(f"Processing url <{url}> cause an exception, url isn't correspond with content.")
                    break
                PageId = int(m_v.group(4))
                if parser.HasContent and (parser.BookName != "" and parser.BookName is not None):
                    AppendContext(parser.BookName, PageId, parser.Content)
            break
        await self.__client_list.ReleaseClient(client_num)
        return

    async def RunCrawler(self):
        while True:
            async with self.__lock_waited_queue:
                if self.__waited_queue.__len__() != 0:
                    url = self.__waited_queue.pop(0)
                    async with self.__lock_visited_table:
                        if self.__visited_table.__contains__(url):
                            # logger.debug(f"meet duplicated url: <{url}>")
                            continue
                        self.__visited_table[url] = True
                    logger.debug(f"process url: <{url}>")
                    num, new_client, _parser = await self.__client_list.GetSpareClient()
                    asyncio.create_task(self.__fetch_and_feed(new_client, url, _parser, num))
                elif self.__client_list.HasRunner():
                    # await asyncio.sleep(1)
                    async with self.__runner_alter_cond:
                        await self.__runner_alter_cond.wait()
                        # logger.debug("wake up from waiting.")
                        continue
                else:
                    break
        return

    async def CloseAll(self):
        await self.__client_list.CloseAll()

    def JoinStartList(self, appendList: list):
        self.__waited_queue += appendList


class HttpClientList():
    def __init__(self, host, port, runner_alter_cond: asyncio.locks.Condition, parserARGS, timeout = 100, num = 10):
        if(num <= 0):
            raise InvalidParameter("num must greater than 0")
        self.__total_num = num
        self.__allocated = 0
        self.__client_list = [aiohttp.client.ClientSession() for i in range(0, self.__total_num)]
        self.__parser_list = [htmlParser(*parserARGS, host) for i in range(0, self.__total_num)]
        self.__alloc_list = [False for _ in range(0, self.__total_num)]
        self.__release_cond = asyncio.locks.Condition()
        self.__num_lock: asyncio.locks.Lock = asyncio.locks.Lock()
        self.__runer_alter_cond: asyncio.locks.Condition = runner_alter_cond

    
    async def CloseAll(self):
        for client in self.__client_list:
            await client.close()


    def __safe_get_client(self):
        assert self.__total_num > self.__allocated
        for i in range(0, self.__total_num):
            if self.__alloc_list[i] == False:
                self.__alloc_list[i] = True
                break
        self.__allocated += 1
        # logger.debug(f"Get Client-{i} success, currently has {self.__total_num - self.__allocated}")
        return (i, self.__client_list[i], self.__parser_list[i])


    def HasRunner(self):
        return self.__allocated != 0


    async def GetSpareClient(self):
        # logger.debug(f"Try to get Client, currently has {self.__total_num - self.__allocated}")
        result = None
        await self.__num_lock.acquire()
        try:
            if self.__allocated == self.__total_num:
                self.__num_lock.release()
                async with self.__release_cond:
                    await self.__release_cond.wait()
                    async with self.__num_lock:
                        assert self.__allocated < self.__total_num
                        result = self.__safe_get_client()
            else:
                result = self.__safe_get_client()
                self.__num_lock.release()
        except Exception:
            self.__num_lock.release
            raise
        assert result is not None
        return result

    
    async def ReleaseClient(self, i):
        assert (i >= 0) and (i < self.__total_num)
        # logger.debug(f"Try to release a client, remind {self.__total_num - self.__allocated} http client avaliable")
        async with self.__num_lock:
            assert self.__alloc_list[i] == True
            self.__alloc_list[i] = False
            self.__allocated -= 1
            # logger.debug(f"releasing client-{i}, remind {self.__total_num - self.__allocated} http client avaliable")

        async with self.__release_cond:
            self.__release_cond.notify()
        async with self.__runer_alter_cond:
            self.__runer_alter_cond.notify()


async def async_crawler_test():
    crawler = CrawlerWhat("http://www.example.com", 80, startLocation="http://www.example.com/", client_num=200)
    await crawler.RunCrawler()
    await crawler.CloseAll()


def crawler_test():
    asyncio.run(bbbbbbbb(async_crawler_test()))


if __name__ == '__main__':
    crawler_test()
    sys.exit(0)
