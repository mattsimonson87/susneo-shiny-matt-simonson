# Benchmark snapshot
- Date: 2025-09-15 13:49:15
- Rows: 500,000
- R: 4.5.1 on mingw32

| expression | median | mem_alloc |
|---|---:|---:|
| canonicalize | 464.08ms |  72.9MB |
| validate |   2.87s |  71.3MB |
| merge_append | 146.63ms |  92.4MB |
| merge_override | 697.9ms | 283.1MB |
