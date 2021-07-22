#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys


try:
    from setuptools import setup, find_packages
except ImportError:
    from distutils.core import setup

if sys.argv[-1] == "publish":
    os.system("python setup.py sdist upload")
    sys.exit()

readme = open("README.rst").read()

setup(
    name="cuerdas",
    version="0.1.0",
    description="Cli tool for bulk editing of stalker ogg vorbis comments.",
    long_description=readme,
    author="Steve Casey",
    author_email="stevencasey21@gmail.com",
    url="https://github.com/sjcasey21/cuerdas",
    packages=find_packages(exclude=["tests"]),
    package_data={"cuerdas": ["*.hy"]},
    include_package_data=True,
    install_requires=["hy >= 1.0a3", "toolz", "regex"],
    license="BSD",
    keywords="cuerdas",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Programming Language :: Lisp",
        "Programming Language :: Python :: 3.7",
    ],
)
