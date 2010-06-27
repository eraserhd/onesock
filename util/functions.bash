
function t() {
	hstest --expose-package=QuickCheck-1.2.0.0 --hide-package=QuickCheck-2.1.0.3
	return $?
}
