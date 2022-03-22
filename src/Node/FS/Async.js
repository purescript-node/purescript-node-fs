export {
  rename as rename_,
  truncate as truncate_,
  chown as chown_,
  chmod as chmod_,
  stat as stat_,
  link as link_,
  symlink as symlink_,
  readlink as readlink_,
  realpath as realpath_,
  unlink as unlink_,
  rmdir as rmdir_,
  mkdir as mkdir_,
  readdir as readdir_,
  utimes as utimes_,
  readFile as readFile_,
  writeFile as writeFile_,
  appendFile as appendFile_,
  exists as exists_,
  open as open_,
  read as read_,
  write as write_,
  close as close_
} from "fs";

export function handleCallbackImpl(left, right, f) {
  return function (err, value) {
    if (err) {
      f(left(err))();
    } else {
      f(right(value))();
    }
  };
}
