import hy
from _pytest.python import Module


def pytest_collect_file(path, parent):
    if path.ext == ".hy" and "test_" in path.basename:
        return Module(path, parent)
