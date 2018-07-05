long hashable_fnv_hash(const unsigned char* str, long len, long salt) {

  unsigned long hash = salt;
  while (len--) {
    hash = (hash * 16777619) ^ *str++;
  }

  return hash;
}

/* Used for ByteArray#s. We can't treat them like pointers in
   native Haskell, but we can in unsafe FFI calls.
 */
long hashable_fnv_hash_offset(const unsigned char* str, long offset, long len, long salt) {
  return hashable_fnv_hash(str + offset, len, salt);
}
