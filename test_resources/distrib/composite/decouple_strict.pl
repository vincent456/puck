import ['fileSystem']
file_kinds = ['File', 'Directory']

r:fileSystem friend-of r:file_kinds
hide file_kinds but-not-from 'fileSystem.CompositeDemo.main__String[]'
