# ----------------------------------------
# Classify remark language (nb/nn)
#
# Adds a language label per remark using langid
# restricted to Norwegian Bokmal and Nynorsk,
# then writes temp/csv/lang_remark.csv.
# ----------------------------------------

import pandas as pd
import langid
from tqdm import tqdm

# Restrict to Norwegian variants
langid.set_languages(["nb", "nn"])

df = pd.read_csv("out/remarks.csv")

# Classify with progress bar
tqdm.pandas()
df["lang"] = df["remark"].progress_apply(lambda x: langid.classify(x)[0])

df.to_csv("temp/csv/lang_remark.csv", index=False)
