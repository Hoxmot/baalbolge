import yaml
import sys
import subprocess
from yaml.loader import SafeLoader

TEST_CASES = 'test-cases.yml'
INTERPRETER_PATH = '../build/baalbolge'


def expected_to_string(expected):
    lines = expected.split('\n')
    expected_string = '\n'.join(map(lambda x: '\\n' if not x else x, lines))
    return '\n<<<<<<<<<<\n{}\n----------\n'.format(expected_string)


def output_to_string(output):
    lines = output.split('\n')
    output_string = '\n'.join(map(lambda x: '\\n' if not x else x, lines))
    return '\n<<<<<<<<<<\n{}\n----------\n'.format(output_string)


class TestRunner:

    def __init__(self, test_cases) -> None:
        self.__good_test_cases = test_cases['good-cases']
        self.__bad_test_cases = test_cases['bad-cases']
        self.__run = 0
        self.__failed = 0
        self.__success = 0
        self.__errors = []

    def run_tests(self) -> None:
        self.__run_good_test_cases()
        self.__run_bad_test_cases()

    def is_success(self):
        return self.__failed == 0

    def report_tests(self):
        print('Tests completed! Run: {}, Success: {}, Failed: {}.'.format(self.__run, self.__success, self.__failed))
        if self.is_success():
            print('SUCCESS')
        else:
            print('FAILURE')
        if self.__errors:
            print('Errors in test cases:\n\t' + '\n\t'.join(self.__errors))

    def __run_good_test_cases(self):
        print('Running cases with expected success for correct Baalbolge code:')
        for test_case in self.__good_test_cases['cases']:
            self.__run += 1
            test_case_prompt = '\t' + test_case['name'] + ':'
            success, msg = self.__good_test(test_case)
            if success:
                self.__success += 1
                print(test_case_prompt, msg)
            else:
                self.__failed += 1
                print(test_case_prompt, msg)
                self.__errors.append(test_case['name'])

    def __good_test(self, test_case):
        file_path = self.__good_test_cases['path'] + test_case['file']
        test_run = subprocess.run([INTERPRETER_PATH, file_path], stdout=subprocess.PIPE, text=True)
        expected_out = file_path + '\n' + str(test_case['out']) + '\n'
        if not test_run.stdout == expected_out:
            return False, 'STDOUT doesn\'t match! Expected:' + expected_to_string(
                expected_out) + ' but got ' + output_to_string(test_run.stdout)
        if not test_run.returncode == test_case['return']:
            return False, 'Return code doesn\'t match! Expected:' + expected_to_string(
                expected_out) + ' but got ' + output_to_string(test_run.returncode)
        return True, 'OK'

    def __run_bad_test_cases(self):
        pass


def read_test_cases(path):
    with open(path) as f:
        return yaml.load(f, Loader=SafeLoader)


def main():
    test_cases = read_test_cases(TEST_CASES)
    test_runner = TestRunner(test_cases)
    test_runner.run_tests()
    test_runner.report_tests()
    if not test_runner.is_success():
        sys.exit(1)


if __name__ == '__main__':
    main()
