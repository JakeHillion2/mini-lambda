import tempfile
import subprocess
import os

TESTS = {
    'ifTest_OneEqualsTwo_HitsElse': (
        '''
func print_int(x);

func main() {
  if 1==2 {
    print_int(4);
  } else {
    print_int(5);
  }
}
''',
        '5\n'
    ),

    'ifTest_NestedIfsOneEqualsTwo_NestsCorrectly': (
        '''
func print_int(x);


func main() {
  if 1==2 {
    if 1==2 {
      print_int(1);
    } else {
      print_int(2);
    }
  } else {
    if 1==1 {
      print_int(3);
    } else {
      print_int(4);
    }
  }
}
        ''',
        '3\n'
    ),
}

BINARY_FILE = '/tmp/__compilers_test_compiled'


def run_test(program_code, expected):
    with tempfile.NamedTemporaryFile() as code_file:
        with open(code_file.name, 'w+') as writeable_file:
            writeable_file.write(program_code)

        try:
            try:
                subprocess.check_call(os.getcwd() + "/lambda -o " + BINARY_FILE + " " + code_file.name, shell=True)
            except subprocess.CalledProcessError:
                print("lambda failed")
                return False

            got_bytes = subprocess.check_output(BINARY_FILE + "; exit 0", shell=True)
            got = got_bytes.decode()

            if got == expected:
                return True
            else:
                print("Expected:\n" + expected)
                print("Got:\n" + got)
                return False
        finally:
            if os.path.exists(BINARY_FILE):
                os.remove(BINARY_FILE)


succeeded = []
failed = []

for (name, parts) in TESTS.items():
    print("Running test '" + name + "'...")
    if run_test(parts[0], parts[1]):
        succeeded.append(name)
    else:
        failed.append(name)

print("Succeeded:  ", succeeded)
print("Failed:     ", failed)
