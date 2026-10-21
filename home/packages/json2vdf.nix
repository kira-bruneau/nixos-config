{ writers, python3Packages }:

writers.writePython3 "json2vdf.py"
  {
    libraries = with python3Packages; [ vdf ];
  }
  ''
    import json
    import sys
    import vdf


    def json2vdf(data):
        if isinstance(data, dict):
            return {k: json2vdf(v) for k, v in data.items()}
        if isinstance(data, list):
            return {str(k): json2vdf(v) for k, v in enumerate(data)}
        else:
            return data


    with open(sys.argv[1]) as fp:
        data = json.load(fp)

    data = json2vdf(data)

    with open(sys.argv[2], "wb") as fp:
        vdf.binary_dump(data, fp)
  ''
