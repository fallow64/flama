#!python3
import sys
import base64
import gzip

COMMAND_TEMPLATE = """/dfgive minecraft:ender_chest{PublicBukkitValues:{"hypercube:codetemplatedata":'{"author":"fallow64","name":"elEvent 6» ePlayer Join Game Event","version":1,"code":"%s"}'},display:{Name:'{"extra":[{"bold":true,"italic":false,"underlined":false,"strikethrough":false,"obfuscated":false,"color":"yellow","text":"Event "},{"bold":false,"italic":false,"color":"gold","text":"» "},{"italic":false,"color":"yellow","text":"Player Join Game Event"}],"text":""}'}} 1"""

def get_template(raw: str) -> str:
    # then gzip compress it
    gzip_compressed = gzip.compress(bytes(raw, 'utf-8'))
    # then b64 encode it
    base64_encoded = base64.b64encode(gzip_compressed)
    # then turn it into a string
    final_code = str(base64_encoded, 'utf-8')

    return final_code


def main():
    pipe = sys.stdin.read()
    print('=' * 80)
    print(pipe)
    print('=' * 80)
    for line in pipe.split('\n'):
        template = get_template(line)
        print(COMMAND_TEMPLATE % template)
        print()


if __name__ == "__main__":
    main()

