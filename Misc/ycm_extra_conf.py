# from distutils.sysconfig import get_python_inc
import platform
import os
import ycm_core
import copy

common_clang_flags = [
    '-Wall',
    '-Wextra',
    '-Werror',
    '-Wno-long-long',
    '-Wno-variadic-macros',
    '-fexceptions',
    '-DNDEBUG'
]

common_linux_header_folder = list(filter(os.path.exists,
                                         ["/usr/include",
                                          "/usr/local/include",
                                          "/usr/share/include"]))

# windows header files handler
mscv_aux_find_level1 = 2
mscv_aux_find_level2 = 2


def list_sub_dir(par):
    return list(filter(lambda x: os.path.isdir(os.path.join(par, x)),
                       os.listdir(par)))


def msvc_level1(par, level):
    if level >= mscv_aux_find_level1:
        return None
    for subdir in list_sub_dir(par):
        if subdir == "VC" or subdir == "VC++":
            return os.path.join(par, subdir)
        else:
            temp_holder = msvc_level1(os.path.join(par, subdir), level + 1)
            if not temp_holder:
                return temp_holder
    return None


def msvc_level2(par, level):
    if level >= mscv_aux_find_level2:
        return None
    for subdir in list_sub_dir(par):
        if subdir == "include":
            return os.path.join(par, subdir)
        else:
            temp_holder = msvc_level1(os.path.join(par, subdir), level + 1)
            if not temp_holder:
                return temp_holder
    return None


def msvc_path():
    if platform.system() != 'Windows':
        return None
    msvs_path = os.getenv("ProgramFiles(x86)") + \
        "/Microsoft Visual Studio"
    if not os.path.exists(msvs_path):
        return None
    msvs_vc_path = msvc_level1(msvs_path, 0)
    if not msvs_vc_path:
        return None
    msvs_vc_include = msvc_level2(msvs_vc_path, 0)
    if not msvs_vc_include:
        return None
    return msvs_vc_include


windows_header_folder = [msvc_path()]


default_c_flags = common_clang_flags + [
    '-x',
    'c',
    '-std=gnu11'
]

default_cpp_flags = common_clang_flags + [
    '-x',
    'c++',
    '-std=gnu++11'
]

linux_c_header_folder = copy.copy(common_clang_flags)
linux_cpp_header_folder = copy.copy(common_linux_header_folder)
for folder in common_clang_flags:
    if os.path.exists(folder + "/c++"):
        if sum(list(map(lambda x: os.path.isfile(folder + "/c++/" + x),
                        os.listdir(folder + "/c++")))) > 0:
            linux_cpp_header_folder += [folder + "/c++"]
        else:
            for _final_cpp in os.listdir(folder + "/c++"):
                if os.path.islink(folder + "/c++/" + _final_cpp):
                    linux_cpp_header_folder += [folder + "/c++/" + _final_cpp]

# c language
c_language = 1
if c_language:
    flags = default_c_flags
else:
    flags = default_cpp_flags

if platform.system() == "Windows":
    append_include_ = windows_header_folder
else:
    if c_language:
        append_include_ = linux_c_header_folder
    else:
        append_include_ = linux_cpp_header_folder

for _include_ in append_include_:
    flags.append('-I')
    flags.append(_include_)

compilation_database_folder = ''

if os.path.exists(compilation_database_folder):
    database = ycm_core.CompilationDatabase(compilation_database_folder)
else:
    database = None

if c_language:
    source_extensions = ['.c']
else:  # cpp
    source_extensions = ['.c', '.cpp', '.cxx', '.hpp', '.cc']


def DirectoryOfThisScript():
    return os.path.dirname(os.path.abspath(__file__))


def IsHeaderFile(filename):
    extension = os.path.splitext(filename)[1]
    return extension in ['.h']


def GetCompilationInfoForFile(filename):
    if IsHeaderFile(filename):
        basename = os.path.splitext(filename)[0]
        for extension in source_extensions:
            replacement_file = basename + extension
            if os.path.exists(replacement_file):
                compilation_info = \
                    database.GetCompilationInfoForFile(replacement_file)
                if compilation_info.compiler_flags_:
                    return compilation_info
            return None
        return database.GetCompilationInfoForFile(filename)


def Settings(**kwargs):
    if not database:
        return {
            'flags': flags,
            'include_paths_relative_to_dir': DirectoryOfThisScript()
        }

    compilation_info = GetCompilationInfoForFile(kwargs["filename"])
    if not compilation_info:
        return None

    final_flags = list(compilation_info.compiler_flags_)

    return {
        'flags': final_flags,
        'include_paths_relative_to_dir': compilation_info.compiler_working_dir_
    }
