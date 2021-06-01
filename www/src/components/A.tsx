import { Link } from "@chakra-ui/layout";

export const A: React.FC<Record<string, any>> = (props) => (
  <Link color="cyan.600" fontWeight="600" {...props} />
);
